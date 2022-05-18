// Future versions of Hyper may add additional config options,
// which will not automatically be merged into this file.
// See https://hyper.is#cfg for all currently supported options.

module.exports = {
  config: {
    // choose either `'stable'` for receiving highly polished,
    // or `'canary'` for less polished but more frequent updates
    updateChannel: 'stable',

    // default font size in pixels for all tabs
    fontSize: 12,

    // font family with optional fallbacks
    fontFamily: 'Cascadia Code PL, Menlo, "DejaVu Sans Mono", Consolas, "Lucida Console", monospace',

    // default font weight: 'normal' or 'bold'
    fontWeight: 'normal',

    // font weight for bold characters: 'normal' or 'bold'
    fontWeightBold: 'bold',

    // line height as a relative unit
    lineHeight: 1,

    // letter spacing as a relative unit
    letterSpacing: 0,

    // terminal cursor background color and opacity (hex, rgb, hsl, hsv, hwb or cmyk)
    cursorColor: 'f8f8f2',

    // terminal text color under BLOCK cursor
    cursorAccentColor: '#000',

    // `'BEAM'` for |, `'UNDERLINE'` for _, `'BLOCK'` for █
    cursorShape: 'BLOCK',

    // set to `true` (without backticks and without quotes) for blinking cursor
    cursorBlink: false,

    // color of the text
    foregroundColor: '#f8f8f2',

    // terminal background color
    // opacity is only supported on macOS
    backgroundColor: '#282a36',

    // terminal selection color
    selectionColor: 'rgba(248,28,229,0.3)',

    // border color (window, tabs)
    borderColor: '#44475a',

    // custom CSS to embed in the main window
    css: `
    .tabs_list .tab_tab.tab_active .tab_text  {
      background: #282a36;
    }

    .tab_active:before {
      border-color: #44475a;
    }
    `,

    // custom CSS to embed in the terminal window
    termCSS: `

    `,

    // if you're using a Linux setup which show native menus, set to false
    // default: `true` on Linux, `true` on Windows, ignored on macOS
    showHamburgerMenu: true,

    // set to `false` (without backticks and without quotes) if you want to hide the minimize, maximize and close buttons
    // additionally, set to `'left'` if you want them on the left, like in Ubuntu
    // default: `true` (without backticks and without quotes) on Windows and Linux, ignored on macOS
    showWindowControls: true,

    // custom padding (CSS format, i.e.: `top right bottom left`)
    padding: '12px 14px',

    // the full list. if you're going to provide the full color palette,
    // including the 6 x 6 color cubes and the grayscale map, just provide
    // an array here instead of a color map object
    colors: {
      black: '#000000',
      red: '#ff5555',
      green: '#50fa7b',
      yellow: '#f1fa8c',
      blue: '#bd93f9',
      magenta: '#ff79c6',
      cyan: '#8be9fd',
      white: 'bfbfbf',
      lightBlack: '#4d4d4d',
      lightRed: '#ff6e67',
      lightGreen: '#5af78e',
      lightYellow: '#f4f99d',
      lightBlue: '#caa9fa',
      lightMagenta: '#ff92d0',
      lightCyan: '#9aedfe',
      lightWhite: '#e6e6e6',
    },

    // the shell to run when spawning a new session (i.e. /usr/local/bin/fish)
    // if left empty, your system's login shell will be used by default
    //
    // Windows
    // - Make sure to use a full path if the binary name doesn't work
    // - Remove `--login` in shellArgs
    //
    // Bash on Windows
    // - Example: `C:\\Windows\\System32\\bash.exe`
    //
    // PowerShell on Windows
    // - Example: `C:\\WINDOWS\\System32\\WindowsPowerShell\\v1.0\\powershell.exe`
    shell: 'C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe',

    // for setting shell arguments (i.e. for using interactive shellArgs: `['-i']`)
    // by default `['--login']` will be used
    shellArgs: ['-NoLogo', '-NoExit'],

    // for environment variables
    env: {},

    // set to `false` for no bell
    bell: 'SOUND',

    // if `true` (without backticks and without quotes), selected text will automatically be copied to the clipboard
    copyOnSelect: true,

    // if `true` (without backticks and without quotes), hyper will be set as the default protocol client for SSH
    defaultSSHApp: true,

    // if `true` (without backticks and without quotes), on right click selected text will be copied or pasted if no
    // selection is present (`true` by default on Windows and disables the context menu feature)
    quickEdit: false,

    // choose either `'vertical'`, if you want the column mode when Option key is hold during selection (Default)
    // or `'force'`, if you want to force selection regardless of whether the terminal is in mouse events mode
    // (inside tmux or vim with mouse mode enabled for example).
    macOptionSelectionMode: 'vertical',

    // URL to custom bell
    // bellSoundURL: 'http://example.com/bell.mp3',

    // Whether to use the WebGL renderer. Set it to false to use canvas-based
    // rendering (slower, but supports transparent backgrounds)
    webGLRenderer: true,

    // for advanced config flags please refer to https://hyper.is/#cfg
    scrollback: 1000,
    windowSize: [540, 380]
  },

  // a list of plugins to fetch and install from npm
  // format: [@org/]project[#version]
  // examples:
  //   `hyperpower`
  //   `@company/project`
  //   `project#1.0.1`
  plugins: [],

  // in development, you can create a directory under
  // `~/.hyper_plugins/local/` and include it here
  // to load it and avoid it being `npm install`ed
  localPlugins: [],

  keymaps: {
    // Example
    // 'window:devtools': 'cmd+alt+o',
    "window:devtools": "alt+f12",
    "window:reload": "alt+f5",
    "window:reloadFull": "alt+f6",
    "window:preferences": "alt+f7",
    "window:hamburgerMenu": "",
    "zoom:reset": "alt+shift+0",
    "zoom:in": "alt+0",
    "zoom:out": "alt+9",
    "window:new": "alt+shift+t",
    "window:minimize": "",
    "window:zoom": "",
    "window:toggleFullScreen": "alt+f11",
    "window:close": [
      "alt+f4"
    ],
    "tab:new": "alt+t",
    "tab:next": [
      "alt+right",
      "ctrl+tab"
    ],
    "tab:prev": [
      "alt+left",
      "ctrl+shift+tab"
    ],
    "tab:jump:prefix": "ctrl",
    "pane:next": [
      "alt+l",
      "alt+j"
    ],
    "pane:prev": [
      "alt+h",
      "alt+k"
    ],
    "pane:splitVertical": "alt+shift+\\",
    "pane:splitHorizontal": "alt+\\",
    "pane:close": "alt+q",
    "editor:undo": "ctrl+shift+z",
    "editor:redo": "ctrl+shift+y",
    "editor:cut": "ctrl+shift+x",
    "editor:copy": "ctrl+shift+c",
    "editor:paste": "ctrl+shift+v",
    "editor:selectAll": "ctrl+shift+a",
    "editor:movePreviousWord": "",
    "editor:moveNextWord": "",
    "editor:moveBeginningLine": "",
    "editor:moveEndLine": "",
    "editor:deletePreviousWord": "",
    "editor:deleteNextWord": "",
    "editor:deleteBeginningLine": "",
    "editor:deleteEndLine": "",
    "editor:clearBuffer": "ctrl+shift+k",
    "editor:break": "ctrl+c",
    "plugins:update": ""
  },
};
