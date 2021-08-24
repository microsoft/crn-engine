// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.FSBOLWrapper.Helper

open FSBOL.Annotation


let append_url (uri:string) (term:string) = 
    match uri.EndsWith "/" with 
    | true -> uri + term + "/"
    | false -> uri + "/" + term + "/" 

let append_id (uri:string) (version:string option) (term:string) = 
    ()

let append_name (str:string option) (term:string) = 
    match str with 
    | Some(s) -> Some(s+"_"+term)
    | None -> None

let append_url_option (uri:string option) (term:string) = 
    match uri with 
    | Some(u) -> Some(append_url u term)
    | None -> None

let create_string_annotation (key:QName) (value:string) = 
    Annotation(key,Literal(String(value)))

let create_double_annotation (key:QName) (value:double) = 
    Annotation(key,Literal(Double(value)))

let create_uri_annotation (key:QName) (uri:string) = 
    Annotation(key,Uri(uri))