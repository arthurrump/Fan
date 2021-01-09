# FFmpegServer

A simple console application that allows web applications to send video to FFmpeg over a websocket connection. This program is BYOF (Bring Your Own FFmpeg), so you need to have it installed on your machine. All FFmpeg configuration options are accessible to the web app, so only run this with trusted applications. 😉

```
USAGE: ffmpegserver [--help] [--port <port>] [--output <path>] [--ffmpeg <path>]

OPTIONS:

    --port, -p <port>     specify the port the server listens on (default: 5000).
    --output, -o <path>   specify the output folder (defaults to current working directory).
    --ffmpeg, -f <path>   specify the path to the FFmpeg executable.
    --help                display this list of options.
```

## API for web apps

The server listens to websocket connections on the root, `localhost:5000/` by default, and requires two query parameters:

- `ffInput`: console arguments passed to FFmpeg before the input specifier. These can be used to specify the input format.
- `ffOutput`: console arguments passed to FFmpeg after the input specifier. These are meant to specify your encoding and output. Note that the client is responsible for choosing an output location, but it is recommended that this is just a filename. Be nice and don't use absolute paths, or go outside the working directory.

The `ffOutput` parameter can be specified more than once, it will then simply be concatenated. This might be useful if you want to specify multiple output streams.

This is the string of arguments as they will be passed to FFmpeg: `-hide_banner {ffInput} -i pipe: {ffOutput}`. The input is set to the stdin pipe, which will be populated with the data sent over the websocket connection. FFmpegServer will reply with the frame number read from FFmpeg's output.
