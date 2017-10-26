const electron = require('electron');
const app = electron.app;
const BrowserWindow = electron.BrowserWindow;
const WebSocket = require('ws');
const dialog = require('electron').dialog;

var Ceramic = {
    dialog: dialog
};

function log(message) {
    console.log('ELECTRON: ' + message);
}

/* Communication */

var RemoteJS = {};

Ceramic.startWebSockets = function(address, port) {
  log('Starting websocket');
  RemoteJS.ws = new WebSocket('ws://' + address + ':' + port);

  RemoteJS.send = function(data) {
    RemoteJS.ws.send(data);
  };

  RemoteJS.ws.onmessage = function(evt) {
    const js = evt.data;
      try {
          log('Eval JS: ' + js);
          eval(js);
    } catch (err) {
      dialog.showErrorBox('JavaScript Error', 'Error evaluating JavaScript from Ceramic: ' + js);
    }
  };
  RemoteJS.ws.onopen = function() {
      log('Websocket connected');
      RemoteJS.send('connected');
  };
};

Ceramic.syncEval = function(id, fn) {
  const result = fn();
  RemoteJS.send(JSON.stringify({
    type: 'response',
    id: id,
    result: result
  }))
};

Ceramic.startCrashReporter = function (options) {
    electron.crashReporter.start(options);
};

/* Windows */

Ceramic.windows = {};

Ceramic.createWindow = function(url, options) {
    log('Creating window');
  var copy = options;
  copy.show = false;

  var win = new BrowserWindow(options);
    if (url) {
        log('Loading url: ' + url);
        win.loadURL(url);
    };
  return win;
};

Ceramic.closeWindow = function(id) {
    log('Closing window');
  Ceramic.windows[id].close()
  Ceramic.windows[id] = null;
};

/* Lifecycle management */

Ceramic.quit = function() {
    log('Quitting');
  app.quit();
};

app.on('window-all-closed', function() {
  RemoteJS.send(
    JSON.stringify({
      type: 'quit'
    })
  );
    
    if (process.platform != 'darwin') {
    // FIXME: signal that everything's closed
  }
});

/* Start up */

app.on('ready', function() {
    log('App connected');
  // Start the WebSockets server
  Ceramic.startWebSockets(process.argv[2],
                          parseInt(process.argv[3]));
});
