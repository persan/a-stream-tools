-------------------------------------------------------------------------------
--                                                                           --
--  Copyright 2015 Per Sandberg <per.s.sandberg@bahnhof.se>                  --
--                                                                           --
--  Permission is hereby granted, free of charge, to any person obtaining a  --
--  copy of this software and associated documentation files                 --
--  (the "Software"), to deal in the Software without restriction, including --
--  without limitation the rights to use, copy, modify, merge, publish,      --
--  distribute, sublicense, and / or sell copies of the Software, and to     --
--  permit persons to whom the Software is furnished to do so, subject to    --
--  the following conditions :                                               --
--                                                                           --
--  The above copyright notice and this permission notice shall be included  --
--  in all copies or substantial portions of the Software.                   --
--                                                                           --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
--  MERCHANTABILITY,                                                         --
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL  --
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR     --
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,    --
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
--  OTHER DEALINGS IN THE SOFTWARE.                                          --
-------------------------------------------------------------------------------
with GNAT.Bounded_Buffers;
with Ada.Streams;
with System;
package Stream_Tools.Bufferd_Streams is

   --  This package provides a task-safe FIFO Buffer with a stream interface.

   type Bufferd_Stream (Capacity : Positive;
                        Ceiling : System.Priority) is
     new Ada.Streams.Root_Stream_Type with private;

   overriding procedure Read
     (Stream : in out Bufferd_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Read the full Stream_Element_Array, if the buffer does not contain enogh
   --  Elements then wait until enogh space is avalible
   --  Will raise constraint error if the buffer is to small to contain the
   --  total amount of elements.

   overriding procedure Write
     (Stream : in out Bufferd_Stream;
      Item   : Ada.Streams.Stream_Element_Array);
   --  Writes the element array to the buffer
   --  If ther is'nt enogh space then wait until there is place.
   --  Will raise constraint error if the buffer is to small to contain the
   --  total amount of elements.

private
   package Stream_Element_Buffers is new GNAT.Bounded_Buffers (Ada.Streams.Stream_Element);
   type Bufferd_Stream (Capacity : Positive;
                        Ceiling : System.Priority)
     is  new Ada.Streams.Root_Stream_Type with record
      Buffer : Stream_Element_Buffers.Bounded_Buffer (Capacity, Ceiling);
   end record;
end Stream_Tools.Bufferd_Streams;
