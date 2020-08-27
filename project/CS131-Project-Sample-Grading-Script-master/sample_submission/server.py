import aiohttp
import time
import json 

API_KEY = 'AIzaSyBnP9bvw074D0b_XFANK-3wM3aL92qhLQs'
server_connections = {
    'Hill': ["Jaquez", "Smith"],
    'Jaquez': ["Hill", "Singleton"],
    'Smith' : ["Hill", "Campbell", "Singleton"],
    'Campbell' : ["Singleton", "Smith"],
    'Singleton' : ["Jaquez", "Campbell", "Smith"]
}
server_ports = {
    'Hill': 11485,
    'Jaquez': 11486,
    'Smith': 11487,
    'Campbell': 11488,
    'Singleton': 11489
}

"""
The code comes from
https://asyncio.readthedocs.io/en/latest/tcp_echo.html
"""

import asyncio
import argparse

class Server:
    def __init__(self, name, port, ip='127.0.0.1', message_max_length=1e6):
        self.name = name
        self.ip = ip
        self.port = port
        self.message_max_length = int(message_max_length)
        self.client_information = {}
        self.messages_received = set()
        self.log = open(f"{name}.log", 'a+')

    async def flood_message(self, message):
        self.messages_received.add(message)
        for server in server_connections[self.name]:
            try:
                reader, writer = await asyncio.open_connection('127.0.0.1', (server_ports[server]))
                self.log.write(f"flooding message from {self.name} to {server}\n")
                self.log.flush()
                writer.write(message.encode())
                await writer.drain()
                writer.close()
            except:
                self.log.write(f"Unable to open connection with {server}\n")

    async def handle_echo(self, reader, writer):
        """
        on server side
        """
        data = await reader.read(self.message_max_length)
        message = data.decode()
        addr = writer.get_extra_info('peername')
        print("{} received {} from {}".format(self.name, message, addr))

        sendback_message = ""
        message_words = message.split()
        num_words = len(message_words)
        if num_words == 4 and message.startswith("IAMAT "):
            client, location, timestamp = message_words[1:]
            diff = time.time() - float(timestamp)
            diff_string = f"+{diff}" if diff >= 0 else diff
            sendback_message = f"AT {self.name} {diff_string} {' '.join(message_words[1:])}"
            self.client_information[client] = (location, sendback_message)
            self.messages_received.add(sendback_message)
            await self.flood_message(sendback_message)
        elif num_words == 4 and message.startswith("WHATSAT "):
            client, radius, upper_bound = message_words[1:]
            radius, upper_bound = int(radius), int(upper_bound)
            if radius > 0 and upper_bound > 0 and radius <= 50 and upper_bound <= 20 and client in self.client_information:
                location, prev_message = self.client_information[client]
                if location.count('+') + location.count('-') != 2:
                    sendback_message = f"? {message}"
                else: 
                    partition = max(location.rfind('+'), location.rfind('-'))
                    latitude, longitude = location[:partition], location[partition:]
                    url = f"https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={API_KEY}&location={latitude},{longitude}&radius={radius}"
                    async with aiohttp.ClientSession(
                        connector=aiohttp.TCPConnector(
                            ssl=False,
                        ),
                    ) as session:
                        async with session.get(url) as resp:
                            response_dict = await resp.json()
                            response_dict['results'] = response_dict['results'][:upper_bound]
                            sendback_message = f"{prev_message}\n{json.dumps(response_dict, indent=4)}"
            else:
                sendback_message = f"? {message}"
        elif num_words == 6 and message.startswith("AT "):
            client, location = message_words[3:5]
            if message not in self.messages_received:
                self.messages_received.add(message)
                self.client_information[client] = (location, message)
                self.log.write(f"Received: {message}")
                self.log.flush()
                await self.flood_message(message)
        else:
            sendback_message = f"? {message}"
            self.log.write(f"Invalid argument: {message}")
        
        if message_words and message_words[0] != "AT":
            print("{} send: {}".format(self.name, sendback_message))
            self.log.write("{} send: {}".format(self.name, sendback_message))
            self.log.flush()
            writer.write(sendback_message.encode())
            await writer.drain()

        writer.close()

    async def run_forever(self):
        server = await asyncio.start_server(self.handle_echo, self.ip, self.port)
        # Serve requests until Ctrl+C is pressed
        print(f'serving on {server.sockets[0].getsockname()}')
        async with server:
            await server.serve_forever()
        # Close the server
        server.close()

def main():
    parser = argparse.ArgumentParser('CS131 project example argument parser')
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()

    print("Hello, welcome to server {}".format(args.server_name))

    server = Server(args.server_name, server_ports[args.server_name])
    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass
    finally:
        server.log.close()


if __name__ == '__main__':
    main()