const http = require("http"); // Loads the http module

http
  .createServer((request, response) => {
    // 1. Tell the browser everything is OK (Status code 200), and the data is in plain text
    response.writeHead(200, {
      "Backend-ID": "3",
      "Content-Type": "text/plain",
    });

    // 2. Write the announced text to the body of the page
    response.write("Hello, World!\n");

    console.log("Request Handled");
    // 3. Tell the server that all of the response headers and body have been sent
    response.end();
  })
  .listen(3002); // 4. Tells the server what port to be on
