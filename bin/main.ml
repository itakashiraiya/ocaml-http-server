open Unix

let create_server_socket port =
  let sockaddr = ADDR_INET (inet_addr_loopback, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock sockaddr;
  listen sock 10;
  sock

let accept_connection sock =
  let has_connection () =
    select [ sock ] [] [] 0.00000000001 |> fun (socks, _, _) ->
    match socks with [ _ ] -> true | [] -> false | _ -> assert false
  in
  let rec aux connections =
    if has_connection () then aux (accept sock |> fun (x, _) -> x :: connections)
    else connections
  in
  aux []

let send_data client_sock data =
  let len = Bytes.length data in
  ignore (send client_sock data 0 len [])

let receive_data client_sock buffer_size =
  let buffer = Bytes.create buffer_size in
  let len = recv client_sock buffer 0 buffer_size [] in
  Bytes.sub buffer 0 len

let close_socket sock = close sock

let _ =
  let sock = create_server_socket 4000 in
  let rec send data = function
    | sock :: rest ->
        send_data sock data;
        send data rest
    | [] -> ()
  in
  let rec loop connections new_connections i =
    match new_connections with
    | [] ->
        let data = "Hello client!" |> Bytes.of_string in
        send data connections;
        let news = accept_connection sock in
        sleep 1;
        loop connections news i
    | x :: xs ->
        Printf.printf "New connection! num %d\n" i;
        flush_all ();
        loop (x :: connections) xs (i + 1)
  in
  loop [] [] 0
