(*
   Client for Amazon Simple Storage Service

   (incomplete)
*)

open Printf
open Lwt
open Log

let service = "s3"

type param = {
  access_key_id: string;
  secret_access_key: string;
  region: string; (* e.g. "us-east-1"; preferably the same as used for EC2 *)
  bucket: string;
}

let make_url_and_headers
    ~param
    ~method_
    ?(content_type = "text/plain")
    ~path
    ?action
    ?(query_param = [])
    ?(body = "")
    () =
  let region = param.region in
  let host = sprintf "%s.%s.amazonaws.com" param.bucket service in
  let query =
    match action with
    | None -> query_param
    | Some s -> (s, []) :: query_param
  in
  let url = Uri.make ~scheme:"https" ~host ~path ~query () in
  let headers =
    Aws_call.make_headers
      ~access_key_id: param.access_key_id
      ~secret_access_key: param.secret_access_key
      ~content_type
      ~http_request_method: method_
      ~host
      ~region
      ~service
      ~path
      ~request_payload: body
      ()
  in
  url, headers

module Http = Util_http_client

let handle_opt_http_response
    parse_response_body
    (resp_status, resp_headers, resp_body) =
  match resp_status with
  | `OK -> Some (parse_response_body resp_body)
  | `Not_found -> None
  | `Bad_request ->
      logf `Error "Bad AWS S3 request. Response body: %s" resp_body;
      failwith "Bad AWS S3 request"
  | err ->
      logf `Error "AWS S3 call failed with error %d: %s\n%!"
        (Cohttp.Code.code_of_status err) resp_body;
      Http_exn.service_unavailable "3rd-party service is unavailable"

let handle_http_response parse_response_body ((_, _, body) as resp) =
  match handle_opt_http_response parse_response_body resp with
  | Some x -> x
  | None ->
      logf `Error "AWS S3 object not found. Response body: %s" body;
      Http_exn.not_found "Resource not found"

let get_object ~param ~path =
  let url, headers = make_url_and_headers ~param ~method_:`GET ~path () in
  Http.get ~headers url >>= fun resp ->
  return (handle_http_response (fun s -> s) resp)

let opt_get_object ~param ~path =
  let url, headers = make_url_and_headers ~param ~method_:`GET ~path () in
  Http.get ~headers url >>= fun resp ->
  return (handle_opt_http_response (fun s -> s) resp)

let put_object ~param ~path ~content_type contents =
  let url, headers = make_url_and_headers ~param ~method_:`PUT ~path () in
  Http.post ~headers url ~body:contents >>= fun resp ->
  return (handle_http_response ignore resp)

let delete_object ~param ~path =
  let url, headers = make_url_and_headers ~param ~method_:`DELETE ~path () in
  Http.delete ~headers url >>= fun resp ->
  return (handle_http_response ignore resp)
