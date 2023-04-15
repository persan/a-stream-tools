pragma Ada_2022;
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
with System;
with Ada.Finalization;
with GNAT.Sockets;
private with Ada.Strings.Text_Buffers;
package Stream_Tools.Memory_Streams is
   use Ada;
   --  Memory_Streams implements stream functionality to be mapped to any
   --  memory location sample of use
   --     declare
   --        Real_Buffer : String (1 .. 1024) := (others => '#');
   --        S           : aliased Memory_Stream;
   --     begin
   --        S.Set_Address (Real_Buffer'Address);
   --        S.Set_Length (Real_Buffer'Length);
   --
   --        String'Write (S'Access, "bulle");
   --        Integer'Write (S'Access, 123);
   --
   --        Memory_Stream'Write(Some_UDP_Stream, S);
   --        -- Write the whole serialized buffer in one transaction.
   --     end;

   type Memory_Stream_Interface is limited interface;
   type Any_Memory_Stream_Interface is access
     all Memory_Stream_Interface'Class;
   --
   procedure Read
     (This   : in out Memory_Stream_Interface;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is abstract;

   procedure Write
     (This   : in out Memory_Stream_Interface;
      Item   : Ada.Streams.Stream_Element_Array) is abstract;

   function Get_Address
     (This : Memory_Stream_Interface) return System.Address is abstract;
   --  Returns the Address to the real buffer

   procedure Set_Address
     (This : in out Memory_Stream_Interface;
      To   :  System.Address) is abstract;
   --  Sets the address to the real buffer.

   function Get_Length
     (This : Memory_Stream_Interface)
      return Ada.Streams.Stream_Element_Count is abstract;
   --  Returns the full length of the buffer.

   procedure Set_Length
     (This : in out Memory_Stream_Interface;
      To   : Ada.Streams.Stream_Element_Count) is abstract;
   --  Sets the full length of the buffer.

   procedure Reset (This : in out Memory_Stream_Interface) is abstract;
   --  moves the stream position to the first element in the stream.

   procedure Seek (This : in out Memory_Stream_Interface;
                   Pos  : Ada.Streams.Stream_Element_Offset) is abstract;
   --  Moves the stream position  forward or backward in the message stream.
   --  Sample:
   --  move to the 10:th element in the message.
   --   Msg.Reset;
   --   Msg.Seek(10);

   function Pos (This : Memory_Stream_Interface)
                 return  Ada.Streams.Stream_Element_Offset is abstract;
   --  Returns the current cursor in the buffer

   function Eof (This : Memory_Stream_Interface) return Boolean is abstract;

   procedure Dump
     (This        : Memory_Stream_Interface;
      Full_Buffer : Boolean := False) is null;
   --  Dumps the contents of the buffer from the first element
   --  to the cursor.

   type Memory_Stream is limited new Ada.Streams.Root_Stream_Type
     and Memory_Stream_Interface
   with private with
     Constant_Indexing => Constant_Reference,
     Variable_Indexing => Reference;

   procedure Read_Memory_Stream
     (This : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out Memory_Stream);

   procedure Write_Memory_Stream
     (This : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Memory_Stream);

   for Memory_Stream'Read use Read_Memory_Stream;
   for Memory_Stream'Write use Write_Memory_Stream;

   type Any_Memory_Stream is access all Memory_Stream'Class;

   overriding
   procedure Set_Address
     (This : in out Memory_Stream; To :  System.Address);
   --  Sets the address to the real buffer.
   overriding
   function Get_Address
     (This : Memory_Stream) return System.Address;

   overriding
   function Get_Length (This : Memory_Stream)
                        return Ada.Streams.Stream_Element_Count;
   --  Returns the full length of the buffer.

   overriding
   procedure Set_Length (This : in out Memory_Stream;
                         To   : Ada.Streams.Stream_Element_Count);
   --  Sets the full length of the buffer.

   overriding
   procedure Reset (This : in out Memory_Stream);
   --  moves the stream position to the first element in the stream.

   overriding
   procedure Seek (This : in out Memory_Stream;
                   Pos  : Ada.Streams.Stream_Element_Offset);
   --  Moves the stream position  forward or backward in the message stream.
   --  Sample:
   --  move to the 10:th element in the message.
   --   Msg.Reset;
   --   Msg.Seek(10);

   overriding
   function Pos (This : Memory_Stream)
                 return  Ada.Streams.Stream_Element_Offset;
   --  Returns the current cursor in the buffer

   overriding
   function Eof (This : Memory_Stream) return Boolean;

   overriding
   procedure Dump
     (This        : Memory_Stream;
      Full_Buffer : Boolean := False);
   --  Dumps the contents of the buffer from the first element
   --  to the cursor to standard output.

   procedure Dump
     (This        : Memory_Stream;
      To          : not null access Ada.Streams.Root_Stream_Type'Class;
      Full_Buffer : Boolean := False);
   --  Dumps the contents of the buffer from the first element
   --  to the cursor as a hex values.

   function As_Standard_String
     (This        : Memory_Stream;
      Full_Buffer : Boolean := False) return String;
   --  returns the content of the buffer as a string.

   procedure As_Source
     (This        : Memory_Stream;
      To          : not null access Ada.Streams.Root_Stream_Type'Class;
      Full_Buffer : Boolean := False);
   --  Prints the content of the buffer as
   --  source code for a Stream_Element_Array.

   overriding procedure Read
     (This   : in out Memory_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (This   : in out Memory_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

   type End_Of_File_Stretegy is
     (Raise_End_Of_File_Exception, Return_Remaing_Data_Or_Raise)
     with
       Default_Value => Raise_End_Of_File_Exception;

   procedure Set_End_Of_File_Stretegy
     (This : in out Memory_Stream;
      To   : End_Of_File_Stretegy := Raise_End_Of_File_Exception);
   --
   --  Defines What should happen when trying to read past end of Buffer.
   --  If Return_Remaing_Data_Or_Raise is set then the normal semantics of read
   --  will change and read will return the remaining Stream_Elements in the
   --  stream, note that this mode will not prevent End_Error if there is no
   --  Stream_Elements to read. A reset of the stream will reset this walue to
   --  Raise_End_Of_File_Exception.

   procedure Send_Socket
     (Socket : GNAT.Sockets.Socket_Type;
      Item   : Memory_Stream;
      Last   : out Ada.Streams.Stream_Element_Offset;
      To     : access GNAT.Sockets.Sock_Addr_Type;
      Flags  : GNAT.Sockets.Request_Flag_Type := GNAT.Sockets.No_Request_Flag);
   --  Sends the whole contents of the stored Stream to a socket.

   type Constant_Reference_Type
      (Element : not null access constant Ada.Streams.Stream_Element) is private
   with
      Implicit_Dereference => Element;

   type Reference_Type (Element : not null access Ada.Streams.Stream_Element) is private
   with
       Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased Memory_Stream;
      Position  : Ada.Streams.Stream_Element_Offset) return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   function Reference
     (Container : aliased in out Memory_Stream;
      Position  : Ada.Streams.Stream_Element_Offset) return Reference_Type;
   pragma Inline (Reference);

   type Expand_Strategy is (As_Needed,
                            Multiply_By_Two,
                            Add_Initial_Size);
   type Dynamic_Memory_Stream
     (Initial_Size : Ada.Streams.Stream_Element_Offset;
      Strategy     : Expand_Strategy) is new Memory_Stream with private;

   overriding procedure Write
     (This   : in out Dynamic_Memory_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

   procedure Read_Dynamic_Memory_Stream
     (This : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out Dynamic_Memory_Stream);

   procedure Write_Dynamic_Memory_Stream
     (This : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Dynamic_Memory_Stream);

   function Input_Dynamic_Memory_Stream
     (This : not null access Ada.Streams.Root_Stream_Type'Class)
      return Dynamic_Memory_Stream;

   procedure Output_Dynamic_Memory_Stream
     (This   : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Dynamic_Memory_Stream);
   for Dynamic_Memory_Stream'Read use Read_Dynamic_Memory_Stream;
   for Dynamic_Memory_Stream'Write use Write_Dynamic_Memory_Stream;
   for Dynamic_Memory_Stream'Input use Input_Dynamic_Memory_Stream;
   for Dynamic_Memory_Stream'Output use Output_Dynamic_Memory_Stream;

private
   subtype Large_Buffer is
     Streams.Stream_Element_Array
       (0 .. Streams.Stream_Element_Offset'Last);
   type Large_Buffer_Access is access Large_Buffer with Storage_Size => 0;

   subtype Large_String is
     String (Positive'First .. Positive'Last);
   type Large_String_Access is access Large_String with Storage_Size => 0;

   type Large_Buffer_Union (Part  : Integer := 0) is record
      case Part is
      when 0 =>
         As_Address       : System.Address;
      when 1 =>
         As_Large_Buffer_Access  : Large_Buffer_Access;
      when others =>
         As_Large_String_Access  : Large_String_Access;
      end case;
   end record with
     Unchecked_Union => True;

   type Memory_Stream is limited new Streams.Root_Stream_Type
     and Memory_Stream_Interface
   with record
      Buffer         : Large_Buffer_Union;
      Buffer_Length  : Streams.Stream_Element_Count  := 0;
      Cursor         : Streams.Stream_Element_Offset := 0;
      On_End_Of_File :  End_Of_File_Stretegy := Raise_End_Of_File_Exception;
   end record with Put_Image => Put_Image;

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; This : Memory_Stream);

   type Controler (Controled : not null access Dynamic_Memory_Stream)
   is new Ada.Finalization.Limited_Controlled with null record;

   overriding procedure Initialize (This : in out Controler);
   overriding procedure Finalize   (This : in out Controler);

   type Dynamic_Memory_Stream (Initial_Size : Streams.Stream_Element_Offset;
                               Strategy     : Expand_Strategy)
   is new Memory_Stream with record
      C            : Controler (Dynamic_Memory_Stream'Access);
   end record;

   procedure Initialize (This : in out Dynamic_Memory_Stream);
   procedure Finalize   (This : in out Dynamic_Memory_Stream);

   procedure Expand
     (This    : in out Dynamic_Memory_Stream;
      To_Size : Ada.Streams.Stream_Element_Offset);
   type Constant_Reference_Type (Element : not null access constant Ada.Streams.Stream_Element) is null record;

   type Reference_Type (Element : not null access Ada.Streams.Stream_Element) is null record;

end Stream_Tools.Memory_Streams;
