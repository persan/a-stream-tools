-------------------------------------------------------------------------------
--                                                                           --
--  Copyright 2016 Per Sandberg <per.s.sandberg@bahnhof.se>                  --
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

with Ada.Streams;
private with Ada.Task_Identification;
package Stream_Tools.Pipes is

   use type Ada.Streams.Stream_Element_Offset;

   type Pipe (Capacity : Ada.Streams.Stream_Element_Offset) is
     new Ada.Streams.Root_Stream_Type with private;
   --  Capacity shall at least be 2 times the
   --  largest item written to the stream.
   --  IE if teh largest item is a 64 bit integer then
   --  the capacity shall be at least 16 bytes.

   overriding procedure Read
     (Stream : in out Pipe;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset) with
     Pre => Item'Length <= Stream.Capacity / 2;
   --  Reads data from the Pipe.
   --  NOTE! only one task is alowed to do consecutive writes
   --   if the write is called from different tasks without resetting the Pipe
   --   a Constraint_Error will be raised.

   overriding procedure Write
     (Stream : in out Pipe;
      Item   :        Ada.Streams.Stream_Element_Array) with
     Pre => Item'Length <= Stream.Capacity / 2;
   --  Reads data from the Pipe.
   --  NOTE! only one task is alowed to do consecutive writes
   --   if the write is called from different tasks without resetting the Pipe
   --   a Constraint_Error will be raised.

   procedure Reset (Stream : in out Pipe);
   --  Resets the Stream to its initial State.

   procedure Close (Stream : in out Pipe);
   --  Closes the Stream and  pending read operations will return with the
   --  remaining data if the data is complete if the data is incomplete
   --  it the read will return 0 elements.
   --  All subsequent write operations will caise

   procedure Dump
     (Stream   : in out Pipe;
      Put_Line : not null access procedure (Item : String));
   --  For debugging.
   --  Dump the contents of the Pipe-Stream using Put_Line.

private

   protected type Internal_Pipe
     (Capacity : Ada.Streams.Stream_Element_Offset) is

      entry Read
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset;
         Caller : Ada.Task_Identification.Task_Id);

      entry Write (Item   : Ada.Streams.Stream_Element_Array;
                   Caller : Ada.Task_Identification.Task_Id);

      procedure Dump (Put_Line : not null access procedure (Item : String));
      procedure Reset;
      procedure Close;

   private
      Buffer       : Ada.Streams.Stream_Element_Array (1 .. Capacity) := [others => 16#CA#];
      Count        : Ada.Streams.Stream_Element_Offset := 0;
      In_Cursor    : Ada.Streams.Stream_Element_Offset := 1;
      Out_Cursor   : Ada.Streams.Stream_Element_Offset := 1;
      Data_Written : Boolean := True;
      Data_Read    : Boolean := True;
      Reader_Task  : Ada.Task_Identification.Task_Id := Ada.Task_Identification.Null_Task_Id;
      Writer_Task  : Ada.Task_Identification.Task_Id := Ada.Task_Identification.Null_Task_Id;
      Is_Closed    : Boolean := False;
   end Internal_Pipe;

   type Pipe
     (Capacity : Ada.Streams.Stream_Element_Offset) is new Ada.Streams .Root_Stream_Type with
      record
         Impl : Internal_Pipe (Capacity);
      end record;

   function Image (Item : Ada.Streams.Stream_Element_Array) return String;

end Stream_Tools.Pipes;
