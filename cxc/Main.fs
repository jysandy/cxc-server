namespace cxc

open WebSharper.Html.Server
open WebSharper
open WebSharper.Sitelets

type Action =
    | [<Method "POST"; Json "numberPlate">] 
      NumberPlate of numberPlate : string
    | [<Method "POST"; Json "personData">]
      PersonalData of personData : PersonData
    | [<Method "POST"; Json "coords">]
      GPSData of coords : Coords
    | CheckOpen
    | [<CompiledName "">] Default
and PersonData =
    {
        phoneNumber: int
        name: string
    }
and Coords =
    {
        x: float
        y: float
    }

///Used for all JSON responses to indicate success or failure
[<NamedUnionCases "result">]
type APIResult<'T> = 
    | [<CompiledName "success">] Success of 'T
    | [<CompiledName "failure">] Failure of message: string

///Returned by GPSData
type JsonStatus = 
    {
        status: string
    }

module ApplicationLogic = 
    type VerificationStatus = 
        | Waiting
        | Verified
        | Denied

    type GateStatus = Open | Closed

    type Status = 
        {
            NumberPlateStatus : VerificationStatus
            PersonDataStatus : VerificationStatus
            CoordsStatus : VerificationStatus
        }

    let waitingStatus = { NumberPlateStatus = Waiting; PersonDataStatus = Waiting; CoordsStatus = Waiting }
    let verifiedStatus = { NumberPlateStatus = Verified; PersonDataStatus = Verified; CoordsStatus = Verified }
    let deniedStatus = { NumberPlateStatus = Denied; PersonDataStatus = Denied; CoordsStatus = Denied }
    let status = ref waitingStatus

    let gateStatus = ref Closed

    let postNumberPlate numberPlate =
        lock status <| fun () ->
            let correctNumberPlate numberPlate = 
                numberPlate = "abcd"
            if correctNumberPlate numberPlate then
                status := { !status with NumberPlateStatus = Verified }
            else
                status := { !status with NumberPlateStatus = Denied }
            Success None

    let postPersonalData personData = 
        lock status <| fun () ->
            let correctPersonData personData = 
                 personData = { phoneNumber = 1234567890; name = "foobar" }
            if correctPersonData personData then 
                status := { !status with PersonDataStatus = Verified }
            else
                status := { !status with PersonDataStatus = Denied }
            Success None
    
    let postGPSData coords =
        lock status <| fun () ->
            let correctCoords coords = 
                coords = (2.0, 2.0)
            if correctCoords coords then
                status := { !status with CoordsStatus = Verified }
            else
                status := { !status with CoordsStatus = Denied }
            
            match !status with
            | s when s = verifiedStatus ->
                status := waitingStatus
                lock gateStatus <| fun () -> gateStatus := Open
                Success { status = "verified" }
            | s when s = deniedStatus ->
                status := deniedStatus
                Success { status = "denied" }
            | _ -> Success { status = "waiting" }

    let getQueryStatus = 
        lock gateStatus <| fun () ->
            match !gateStatus with
            | Open -> 
                gateStatus := Closed
                "open"
            | Closed -> "closed"

type WebApi() = 
    interface IWebsite<Action> with
        member this.Sitelet = 
            Sitelet.Infer <| function
                | NumberPlate numPlate -> 
                    Content.JsonContent <| fun _ ->
                        ApplicationLogic.postNumberPlate numPlate
                | PersonalData perData ->
                    Content.JsonContent <| fun _ ->
                        ApplicationLogic.postPersonalData perData
                | GPSData( {x = x; y = y} ) ->
                    Content.JsonContent <| fun _ ->
                        ApplicationLogic.postGPSData(x, y)
                | CheckOpen -> 
                    Content.JsonContent <| fun _ ->
                        ApplicationLogic.getQueryStatus
                | Default -> Content.PageContent <| fun _ -> Page.Default
        member this.Actions = []

type Global() = 
    inherit System.Web.HttpApplication()

[<assembly: WebsiteAttribute(typeof<WebApi>)>]
do ()