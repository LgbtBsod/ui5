const { spawn } = require("node:child_process");

const executable = process.platform === "win32" ? "python.exe" : "python";

const child = spawn(executable, ["scripts/dev_static_server.py"], {
  stdio: "inherit"
});

child.on("exit", (code, signal) => {
  if (signal) {
    process.kill(process.pid, signal);
    return;
  }
  process.exit(typeof code === "number" ? code : 1);
});
