﻿module XxoO.NightMode

open Xamarin.Forms
open XxoO.Domain

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

let cellColor cellStatus nightMode =
    match cellStatus with
    | Played PlayerX -> nightModeSafePlayerXCell nightMode
    | Played PlayerO -> nightModeSafePlayerOCell nightMode
    | Empty -> nightModeSafeEmptyCell nightMode

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