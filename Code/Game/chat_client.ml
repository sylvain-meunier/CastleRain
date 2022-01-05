#use "topfind";;
#load "unix.cma";;

module Client =
struct
  let serverip = "127.0.1.1"
  let serverport = 2400

  let open_connection sockaddr =
    let domain = Unix.domain_of_sockaddr sockaddr in
    let sock = Unix.socket domain Unix.SOCK_STREAM 0 
    in try Unix.connect sock sockaddr ;
           (Unix.in_channel_of_descr sock , Unix.out_channel_of_descr sock)
      with exn -> (Unix.close sock ; raise exn)
 
  let shutdown_connection inchan =
    Unix.shutdown (Unix.descr_of_in_channel inchan) Unix.SHUTDOWN_SEND
  
  let main_client client_fun =
      let serveur_adr = Unix.inet_addr_of_string serverip in
      let sockadr = Unix.ADDR_INET(serveur_adr, serverport) in 
      let ic,oc = open_connection sockadr in
      (client_fun ic oc ; shutdown_connection ic)
 
  let client_fun ic oc = 
    try
      output_string oc "Coucou" ;
      (* Flush n'a pas d'effet sur les sockets ? *)
      flush oc ;
      Unix.sleepf 2. ;
      flush oc ;
      Unix.sleepf 3. ;
    with 
      | Exit -> exit 0
      | exn -> (shutdown_connection ic ; raise exn)
 
  let launch () = main_client client_fun

 end ;;

 Client.launch () ;;