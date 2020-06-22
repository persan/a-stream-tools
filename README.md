# Ada-stream-tools

Provides a set of utilities for streams such as:

 * Streams in memory to be able to serialize to memory and send all as a transaction.
 * Stream FIFO.
 * Recording streams (saves output to a file with timestamps.


`````Ada
 declare
   Real_Buffer : String (1 .. 1024) := (others => '#');
   S           : aliased Memory_Stream;
 begin
   S.Set_Address (Real_Buffer'Address);
   S.Set_Length (Real_Buffer'Length);
   String'Write (S'Access, "funky");
   Integer'Write (S'Access, 123);
   Memory_Stream'Write(Some_UDP_Stream, S);
   --  Write the whole serialized buffer in one transaction.
 end;
`````

Tested with:
 * GNATPro 21.0w-20200507
 * GNATPro 19.0
 * GNATPro 7.4
 * GCC 4.9.3(Raspian)

Installation
make
sudo make install
