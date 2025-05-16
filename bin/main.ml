open Unix

let create_server_socket port =
  let sockaddr = ADDR_INET (inet_addr_loopback, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  bind sock sockaddr;
  listen sock 10;
  sock

let accept_connection sock = accept sock |> fun (x, _) -> x

let send_data client_sock data =
  let len = Bytes.length data in
  ignore (send client_sock data 0 len [])

let receive_data client_sock buffer_size =
  let buffer = Bytes.create buffer_size in
  let len = recv client_sock buffer 0 buffer_size [] in
  Bytes.sub buffer 0 len

let close_socket sock = close sock

let receive_msg client =
  (try Ok (receive_data client 4) with Unix.Unix_error (e, _, _) -> Error e)
  |> function
  | Ok size when Bytes.length size = 4 ->
      let size = size |> String.of_bytes |> int_of_string in
      receive_data client size |> fun msg ->
      assert (Bytes.length msg = size);
      msg |> String.of_bytes |> fun x -> Some x
  | Ok size when Bytes.length size = 0 -> None
  | Ok size ->
      prerr_bytes size;
      assert false
  | Error err ->
      error_message err |> print_endline;
      assert false

let rec process_client_echo client =
  receive_msg client |> function
  | Some msg ->
      Bytes.of_string msg |> send_data client;
      process_client_echo client
  | None -> ()

(* let process_client_test client = *)
(*   let rec send () = *)
(*     send_data client (Bytes.of_string "Hellx client!"); *)
(*     Unix.sleep 1; *)
(*     send () *)
(*   in *)
(*   let rec receive () = *)
(*     receive_msg client |> function *)
(*     | Some msg -> *)
(*         print_endline msg; *)
(*         receive () *)
(*     | None -> print_endline "Bye client!" *)
(*   in *)
(*   match Unix.fork () with 0 -> send () | _ -> receive () *)

let rec main sock =
  let client = accept_connection sock in

  match Unix.fork () with
  | 0 ->
      ignore (Sys.set_signal Sys.sigint Sys.Signal_default);
      process_client_echo client
  | _ -> main sock

let _ =
  let sock = create_server_socket 4000 in
  ignore
    (Sys.set_signal Sys.sigint
       (Sys.Signal_handle
          (fun _ ->
            print_endline "intr handler!";
            Unix.close sock;
            exit 0)));
  main sock
