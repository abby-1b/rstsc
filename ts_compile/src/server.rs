use std::thread;
use websocket::{sync::Server, OwnedMessage};
use crate::compiler;

pub fn start_server() {
  let server = Server::bind("localhost:7787").unwrap();
  println!("WebSocket server initialized.");
  for request in server.filter_map(Result::ok) {
    thread::spawn(|| handle_connection(request));
  }
}

fn handle_connection(request: websocket::server::upgrade::WsUpgrade<
  std::net::TcpStream, Option<websocket::sync::server::upgrade::Buffer>
>) {
  let mut client = request
    .use_protocol("rust-websocket").accept().unwrap();

  // Get ip (and log)
  let ip = client.peer_addr().unwrap();
  println!("Connection from {}", ip);

  // Let client know they're connected
  let message = OwnedMessage::Text("alive".to_string());
  client.send_message(&message).unwrap();

  // Start receive loop
  let (mut receiver, mut sender) = client.split().unwrap();
  for message in receiver.incoming_messages() {
    if message.is_err() { continue; } // Ignore errors
    let message = message.unwrap();

    match message {
      OwnedMessage::Text(data) => {
        // Send response
        println!("Compile request from {}", ip);
        let out_code = compiler::compile(&data).unwrap();
        println!("Compiled: {}", out_code);
        let response = OwnedMessage::Text(out_code);
        sender.send_message(&response).unwrap();
      }

      OwnedMessage::Close(_) => {
        let response = OwnedMessage::Close(None);
        sender.send_message(&response).unwrap();
        println!("Client {} disconnected", ip);
        return;
      }

      OwnedMessage::Ping(ping) => {
        let response = OwnedMessage::Pong(ping);
        println!("Ping from {}", ip);
        sender.send_message(&response).unwrap();
      }

      _ => {
        println!("Unknown message from {}", ip);
        let response = OwnedMessage::Text(format!(
          "Unknown message: {:?}",
          message
        ));
        sender.send_message(&response).unwrap();
      },
    }
  }
}
