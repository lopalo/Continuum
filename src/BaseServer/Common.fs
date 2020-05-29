module Continuum.BaseServer.Common

open Continuum.Core.Types
open Continuum.BaseServer

type Object =
    {Label : Label
     mutable Position : Position}

type Objects = Map<ObjectId, Object>


type Context =
    {NodeId : NodeId
     Clients : Clients
     mutable Objects : Objects}
