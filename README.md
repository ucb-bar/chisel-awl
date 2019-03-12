High BandWidth InterFace (HBWIF)
=======================

HBWIF is an interface generator for high-speed serial links that leverages
scala's powerful functional programming capabilities.

A link is composed of:
 - Analog frontend
 - Encoder/Decoder
 - Packetizer (Backend)
 - Controller

The user is responsible for suppling the wrapper around the analog frontend,
which must implement the abstract methods in `Transceiver.scala`.

The user may supply a custom encoder/decoder or use the provided 8b/10b codec.
See `Encoding.scala` for the abstract base class or `Encoding8b10b.scala` for
the 8b/10b implementation.

The user may supply a custom packetizer (digital backend) or use one of the
provided backends. Currently supported backends are TileLink (using diplomacy)
or FixedWidth (a decoupled interface- ready, valid, data). See
`Packetization.scala` for the abstract base class, `tilelink/TLPacketizer.scala`
for the TileLink implementation, or `FixedWidthPacketization.scala` for the
FixedWidth implementation.

The controller is the mechanism for reading and writing control and status
registers. One controller is provided: a memory-mapped register-based
interface using TileLink (using diplomacy). The user may also
provide a custom controller if desired. See `Controller.scala` for the abstract
base class or `tilelink/TLController.scala` for the TileLink implementation.

The aforementioned components are connected in `Lane.scala`. The desired
variant of each component is specified by implementing a specific generator
method. The provided implementations each provide traits which implement this
method for you- it is recommended to use these traits unless you plan on
writing a custom implementation of the component. For an example, look at
`tilelink/TLLane.scala`.

