module Continuum.BaseServer.Common

open Continuum.Core
open Continuum.Core.Types
open Continuum.BaseServer

type Object =
    {Label : Label
     mutable Position : Position}

type Objects = Map<ObjectId, Object>


type Context =
    {NodeId : NodeId
     RemoteBaseNodes : RemoteNodes<Message.BaseBase>
     Clients : Clients
     mutable Objects : Objects}
