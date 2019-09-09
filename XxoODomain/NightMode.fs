module XxoODomain.NightMode

open Xamarin.Forms
open XxoODomain.Domain

let nightModeSafePlayerXCell nightMode =
    if nightMode
    then Color.FromHex "3B6E67"
    else Color.LightSkyBlue

let nightModeSafePlayerOCell nightMode =
    if nightMode
    then Color.FromHex "873831"
    else Color.Orange

let nightModeSafePlayerColor nightMode player =
    match player with
    | PlayerX -> nightModeSafePlayerXCell nightMode
    | PlayerO -> nightModeSafePlayerOCell nightMode

let nightModeSafeEmptyCell nightMode =
    if nightMode
    then Color.FromHex "DDD4E7"
    else Color.AntiqueWhite

let nightModeSafeTextColor nightMode =
    if nightMode
    then Color.WhiteSmoke
    else Color.Black

let nightModeSafeGridSelectionBackgroundColor nightMode =
    if nightMode
    then Color.ForestGreen
    else Color.LightSeaGreen

let backgroundColor nightMode =
    if nightMode
    then Color.FromHex "#243447"
    else Color.White