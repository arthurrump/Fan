module FFmpeg

open Browser.Types
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open System

type LogParams =
    abstract ``type`` : string with get, set
    abstract message : string with get, set

type ProgressParams =
    abstract ratio : float with get, set

type FFmpeg =
    /// Load ffmpeg.wasm-core script.
    /// In browser environment, the ffmpeg.wasm-core script is fetch from
    /// CDN and can be assign to a local path by assigning `corePath`.
    /// In node environment, we use dynamic require and the default `corePath`
    /// is `$ffmpeg/core`.
    ///
    /// Typically the load() func might take few seconds to minutes to complete,
    /// better to do it as early as possible.
    abstract load : unit -> Promise<unit>
    /// Determine whether the Core is loaded.
    abstract isLoaded : unit -> bool
    /// Run ffmpeg command.
    /// This is the major function in ffmpeg.wasm, you can just imagine it
    /// as ffmpeg native cli and what you need to pass is the same.
    ///
    /// For example, you can convert native command below:
    ///
    /// ```
    /// $ ffmpeg -i video.avi -c:v libx264 video.mp4
    /// ```
    ///
    /// To
    ///
    /// ```
    /// await ffmpeg.run('-i', 'video.avi', '-c:v', 'libx264', 'video.mp4');
    /// ```
    abstract run: [<ParamArray>] args: string[] -> Promise<unit>
    ///
    abstract FS : method : string * [<ParamArray>] args : obj[] -> obj
    /// a function to trace the progress
    abstract setProgress: progress: (ProgressParams -> unit) -> unit
    /// a function to get log messages
    abstract setLogger: log: (LogParams -> unit) -> unit
    /// turn on or off all logs
    abstract setLogging: logging: bool -> unit

type CreateFFmpegOptions =
    /// path for ffmpeg-core.js script
    abstract corePath: string with get, set
    /// a boolean to turn on all logs, default is false
    abstract log: bool with get, set
    /// a function to get log messages, a quick example is ({ message }) => console.log(message)
    abstract logger : (LogParams -> unit) with get, set
    /// a function to trace the progress, a quick example is p => console.log(p)
    abstract progress : (ProgressParams -> unit) with get, set

type FFmpegType =
    /// Create ffmpeg instance.
    /// Each ffmpeg instance owns an isolated MEMFS and works
    /// independently.
    /// 
    /// For example:
    /// 
    /// ```
    /// const ffmpeg = createFFmpeg({
    ///  log: true,
    ///  logger: () => {},
    ///  progress: () => {},
    ///  corePath: '',
    /// })
    /// ```
    /// 
    /// For the usage of these four arguments, check config.js
    abstract createFFmpeg : ?options : CreateFFmpegOptions -> FFmpeg
    /// Helper function for fetching files from various resource.
    /// Sometimes the video/audio file you want to process may located
    /// in a remote URL and somewhere in your local file system.
    /// 
    /// This helper function helps you to fetch to file and return an
    /// Uint8Array variable for ffmpeg.wasm to consume.
    abstract fetchFile : data: U4<string, Buffer, Blob, File> -> Uint8Array

let FFmpeg : FFmpegType = importAll "@ffmpeg/ffmpeg"
