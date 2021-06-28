namespace Mimir.Jsonic

type Api<'input, 'output> =
    {
        InputCodec:Codec<'input>
        OutputCodec:Codec<'output>
    }

type ApiDefinition<'apiUnion> =
    private {
        UnionToNameMap: UnionMap<'apiUnion, string>
        NameToUnionMap: Map<string, 'apiUnion>
    }

[<RequireQualifiedAccess>]
module Api =
    open System.Reflection
    open FSharp.Reflection

    let empty =
        { UnionToNameMap = UnionMap.empty
          NameToUnionMap = Map.empty
        }

    let add (apiCtor:Api<'input, 'output> -> 'apiUnion)
            (inputCodec:Codec<'input>)
            (outputCodec:Codec<'output>)
            (definition:ApiDefinition<'apiUnion>) =

        let api =
            { InputCodec = inputCodec
              OutputCodec = outputCodec
            }

        let apiUnion = apiCtor api
        let (apiUnionInfo,_) = FSharpValue.GetUnionFields(apiUnion, typeof<'apiUnion>, BindingFlags.Public ||| BindingFlags.NonPublic)
        let apiName = apiUnionInfo.Name


        { definition with
            UnionToNameMap =
                definition.UnionToNameMap
                |> UnionMap.add apiCtor api apiName

            NameToUnionMap =
                definition.NameToUnionMap
                |> Map.add apiName (apiCtor api)
        }

    let tryFind (apiCtor:Api<'input, 'output> -> 'apiUnion)
                (definition:ApiDefinition<'apiUnion>) =

        definition.UnionToNameMap
        |> UnionMap.tryFind apiCtor
        |> Option.map(fun (a, b) -> (b, a))

    let getWrappers (definition:ApiDefinition<'apiUnion>)=
        definition.NameToUnionMap
        |> Map.toList

    let tryFindWrapper name (definition:ApiDefinition<'apiUnion>)=
        definition.NameToUnionMap
        |> Map.tryFind name
