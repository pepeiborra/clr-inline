

open System

module Introspect =
    type FlatTypeName = 
         { name:string; args:FlatTypeName[]}
        static member Create (typ:Type) : FlatTypeName =
            { name = if typ.IsPrimitive then typ.Name else if typ.IsGenericType then let n = typ.GetGenericTypeDefinition().FullName in n.[0..n.Length-3] else typ.FullName
            ; args = (if typ.IsGenericType then typ.GetGenericArguments() else [||]) |> Array.map FlatTypeName.Create }
        override this.ToString() =
            let args = if this.args.Length = 0 then "" else sprintf "<%s>" (String.Join(",",this.args))
            this.name + args

    let introspect () : array<string * bool * FlatTypeName> =
        [| for m in typeof<Generated.Intro>.DeclaringType.GetMethods() do
            if m.Name.StartsWith("inline") then
                let ret = m.ReturnType
                yield (m.Name,ret.IsArray,FlatTypeName.Create ret)
         |]
