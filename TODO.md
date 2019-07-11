# Iteratia 1

- downloadeaza blockurile in ordine consecutiva
- tine toate blocurile in memorie si scrie la final in fisier
- download un singur fisier
- no upload
- mesageje s-ar putea sa nu vina in pachete disceret, ele s-ar putea sa fie
  despartite in mai multe pachete. Trebuie un listener care asteapta pana
  construieste un mesaj complet si apoi il livreaza. Alta posibilitate este
  ca un pachet sa contina mai multe mesaje, deci trebuie despartite in
  mesaje discrete.

# Future:

- UI cu Elm
- un algoritm mai bun de selectia a urmatorului block pentru download
- nu tine toate blocurile in memorie
- comunica cu trackerul prin UDP
- download un torrent cu mai multe fisier
- handle upload
- reconnect to more peers when they drop

I need a central unit to spawn and manage all peer threads.
Get peers from tracker
Spawn threads for maxConnections peers
Disable and enable peer threads by using choke and interested

# How a single peer should work:

- has inbound and outbound parts
     - inbound: handle messages from remote peer
            - update KeepAlive status of remote peer, no KeepAlive for two minutes => drop connection
            - on BitField
                bitfield has wrong length or spare bits at the end are set => drop connection
            - on Request, length != 2^14 => drop connection 
     - outbound: send messages to remote peer
         - send KeepAlive every 2 minutes

- avoid fibrilation by changing choked peers every 10 seconds
- optimistic unchoking => every 30 seconds

```
if amInterested
    if not amChoked
        if have piece to download
            request piece to remote peer
            download piece
            send piece to pieces manager
        else
            request piece from pieces manager
```

## Requesting pieces

- request only pieces that the peer has => Peer has to store pieces that connected peer has

## PiecesManager

- on torrent load build an array of booleans that shows which pieces we have so far
- has a inbound message queue from Peers
- has n outbound message queues, one for each Peer
- messages:
    inbound: RequestPiece - peer request a piece to download 
             CompletePiece ByteString - Peer sends completed piece
    outbound: RequestePieceIndex Int - PiecesManager sends index of next piece to download
- on CompletePiece validate piece SHA-1 hash

- RequestedPiece ByteString -> return to Peer data of requested piece, used for uploading to other peers
