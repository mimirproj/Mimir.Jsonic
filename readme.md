# Mimir.Jsonic

With Jsonic you can declare a Codec with which to encode and decode typed data as JSON. You can then use the Codec you've written to enable RPC between Client and Server.

This library differs from others in that you have full control over how the data is encoded as no auto-serialization is done.


## Getting started

### Create a shared type and codec for use from both the client and server

```
open Mimir.Jsonic
open Mimir.Jsonic.Net

type Person =
    { Name: string
      Surname: string
    }

    static member Codec:Codec<Person> =
        Codec.object (fun name surname ->
            { Name = name
              Surname = surname
            })
        |> Codec.field "name" (fun v -> v.Name) Codec.string
        |> Codec.field "surname" (fun v -> v.Surname) Codec.string
        |> Codec.buildObject


// Describe the API as a discriminated union
type ApiUnion =
    | SayHello of Api<Person, string>


// Add the type definition and codecs to the API definition
let apiDefinition =
    Api.empty
    |> Api.add SayHello Person.Codec Codec.string
```


### Implement the API in Saturn (Server-Side)
```
open Mimir.Jsonic.Saturn
open Saturn

let apiHandler =
    Api.build apiDefinition (fun apis ->
        match apis with
        | SayHello api ->
            Api.route api (fun input ->
                task {
                    return $"Hello {input.Name} {input.Surname}."
                }
            )
    )

router {
    forward "api" apiHandler
}
```

### Call the API over http
```
open Mimir.Jsonic
open Mimir.Jsonic.Net

let url = "localhost:8085/api"

// Call from client
let person = { Name="Bob"; Surname="Smith" }
let sayHelloAsync = Api.callAsync (Api.jsonOverHttp url) apiDefinition SayHello

task {
    let! response = sayHelloAsync person // Hello Bob Smith

    ...
}
```


That is all that is needed to perform an RPC call.