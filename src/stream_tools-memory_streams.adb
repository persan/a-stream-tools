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

--                                                                          --
------------------------------------------------------------------------------
with Ada.IO_Exceptions;
with GNAT.Memory_Dump;
with System.Memory;
package body Stream_Tools.Memory_Streams is
   use Ada.Streams;
   overriding
   procedure Dump
     (This        : Memory_Stream;
      Full_Buffer : Boolean := False) is
      Buffer : Large_Buffer_Access renames This.Buffer.As_Pointer;
      use  GNAT.Memory_Dump;
   begin
      if Full_Buffer then
         GNAT.Memory_Dump.Dump
           (Addr   => Buffer.all (Buffer.all'First)'Address,
            Count  => Natural (This.Buffer_Length),
            Prefix => Offset);
      else
         GNAT.Memory_Dump.Dump
           (Addr   => Buffer.all (Buffer.all'First)'Address,
            Count  => Natural ((This.Cursor) - 1),
            Prefix => Offset);
      end if;
   end Dump;

   overriding function Eof (This : Memory_Stream) return Boolean is
   begin
      return This.Cursor > This.Buffer_Length;
   end Eof;

   -----------------
   -- Get_Address --
   -----------------

   overriding
   function Get_Address (This : Memory_Stream) return System.Address is
   begin
      return This.Buffer.As_Address;
   end Get_Address;

   ----------------
   -- Get_Length --
   ----------------

   overriding function Get_Length (This : Memory_Stream)
                                   return Ada.Streams.Stream_Element_Count is
   begin
      return This.Buffer_Length;
   end Get_Length;

   overriding procedure Seek (This : in out Memory_Stream;
                              Pos  : Ada.Streams.Stream_Element_Offset) is
   begin
      This.Cursor := This.Cursor + Pos;
   end Seek;

   overriding function Pos (This : Memory_Stream)
                            return  Ada.Streams.Stream_Element_Offset is
   begin
      return This.Cursor;
   end Pos;

   -----------------
   -- Set_Address --
   -----------------

   overriding
   procedure Set_Address
     (This : in out Memory_Stream; To :  System.Address) is
   begin
      This.Buffer.As_Address := To;
   end Set_Address;

   ----------------
   -- Set_Length --
   ----------------

   overriding
   procedure Set_Length (This : in out Memory_Stream;
                         To   : Ada.Streams.Stream_Element_Count)  is
   begin
      This.Buffer_Length := To;
      This.Reset;
   end Set_Length;

   ----------
   -- Read --
   ----------

   overriding
   procedure Read
     (This   : in out Memory_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      First  : Stream_Element_Offset;
      LLast  : Stream_Element_Offset;
   begin
      First :=  This.Cursor;
      LLast := This.Cursor + Item'Length - 1;
      if LLast > This.Buffer_Length then
         if This.Cursor > This.Buffer_Length then
            raise  Ada.IO_Exceptions.End_Error;
         elsif This.On_End_Of_File = Raise_End_Of_File_Exception  then
            raise  Ada.IO_Exceptions.End_Error;
         else
            Item (Item'First .. Item'First + This.Buffer_Length - First - 1) :=
              This.Buffer.As_Pointer.all (First .. This.Buffer_Length - 1);
            Last := Item'First + This.Buffer_Length - First - 1;
            This.Cursor := This.Buffer_Length + 1;
         end if;
      else
         Item := This.Buffer.As_Pointer.all (First .. LLast);
         This.Cursor := LLast + 1;
         Last := Item'Last;
      end if;
   end Read;

   procedure Set_End_Of_File_Stretegy
     (This : in out Memory_Stream;
      To   : End_Of_File_Stretegy := Raise_End_Of_File_Exception) is
   begin
      This.On_End_Of_File := To;
   end Set_End_Of_File_Stretegy;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write
     (This   : in out Memory_Stream;
      Item   : Stream_Element_Array)
   is
      First : Stream_Element_Offset;
      Last  : Stream_Element_Offset;
   begin
      First :=  This.Cursor;
      Last := This.Cursor + Item'Length - 1;
      if Last > This.Buffer_Length then
         raise  Ada.IO_Exceptions.Device_Error;
      end if;
      This.Cursor := Last + 1;
      This.Buffer.As_Pointer.all (First .. Last) := Item;
   end Write;

   overriding
   procedure Reset (This : in out Memory_Stream) is
   begin
      This.Cursor := This.Buffer.As_Pointer.all'First;
      This.On_End_Of_File := Raise_End_Of_File_Exception;
   end Reset;

   procedure Read_Memory_Stream
     (This   : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Memory_Stream) is
      pragma Unreferenced (This, Item);
   begin
      raise Program_Error with
        "Its not possible to read into a memory stream using 'read";
   end Read_Memory_Stream;

   procedure Write_Memory_Stream
     (This   : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Memory_Stream) is
   begin
      Ada.Streams.Stream_Element_Array'Write
        (This,
         Item.Buffer.As_Pointer.all
           (Item.Buffer.As_Pointer.all'First .. Item.Cursor - 1));
   end Write_Memory_Stream;

   procedure Read_Dynamic_Memory_Stream
     (This   : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Dynamic_Memory_Stream) is
   begin
      Read_Memory_Stream (This, Memory_Stream (Item));
   end Read_Dynamic_Memory_Stream;

   function Input_Dynamic_Memory_Stream
     (This : not null access Ada.Streams.Root_Stream_Type'Class)
      return Dynamic_Memory_Stream is
      Initial_Size : Ada.Streams.Stream_Element_Offset;
      Strategy     : Expand_Strategy;
   begin
      Ada.Streams.Stream_Element_Offset'Read (This, Initial_Size);
      Expand_Strategy'Read (This, Strategy);
      return Ret : Dynamic_Memory_Stream (Initial_Size, Strategy) do
         Ada.Streams.Stream_Element_Array'Write (This, Ret.Buffer.As_Pointer.all (0 .. Initial_Size - 1));
      end return;
   end Input_Dynamic_Memory_Stream;

   procedure Output_Dynamic_Memory_Stream
     (This   : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Dynamic_Memory_Stream) is
   begin
      Ada.Streams.Stream_Element_Offset'Write (This, Item.Cursor - 1);
      Expand_Strategy'Write (This, Item.Strategy);
      Ada.Streams.Stream_Element_Array'Write (This, Item.Buffer.As_Pointer.all (0 .. Item.Cursor - 1));
   end Output_Dynamic_Memory_Stream;

   procedure Write_Dynamic_Memory_Stream
     (This   : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Dynamic_Memory_Stream) is
   begin
      Write_Memory_Stream (This, Memory_Stream (Item));
   end Write_Dynamic_Memory_Stream;

   overriding procedure Write
     (This   : in out Dynamic_Memory_Stream;
      Item   : Ada.Streams.Stream_Element_Array) is
   begin
      if This.Cursor + Item'Length > This.Buffer_Length then
         This.Expand (This.Cursor + Item'Length);
      end if;
      Memory_Stream (This).Write (Item);
   end Write;

   procedure Expand
     (This    : in out Dynamic_Memory_Stream;
      To_Size : Ada.Streams.Stream_Element_Offset) is
      New_Size : System.Memory.size_t := 0;
      use System.Memory;
   begin
      while New_Size < size_t (To_Size) loop
         case This.Strategy is
         when As_Needed =>
            New_Size := size_t (To_Size);
         when Multiply_By_Two =>
            New_Size := size_t (2 * This.Buffer_Length);
         when Add_Initial_Size =>
            New_Size := size_t (This.Buffer_Length + This.Initial_Size);
         end case;
      end loop;
      This.Buffer.As_Address :=  System.Memory.Realloc
        (This.Buffer.As_Address, New_Size);
      This.Buffer_Length := Streams.Stream_Element_Count (New_Size);
   end Expand;

   procedure Initialize (This : in out Dynamic_Memory_Stream) is
      use System.Memory;
   begin
      This.Buffer.As_Address :=
        System.Memory.Alloc (size_t (This.Initial_Size));
      This.Buffer_Length := This.Initial_Size;
   end Initialize;

   procedure Finalize   (This : in out Dynamic_Memory_Stream) is
   begin
      System.Memory.Free (This.Buffer.As_Address);
   end Finalize;

   overriding procedure Initialize (This : in out Controler) is
   begin
      This.Controled.Initialize;
   end Initialize;

   overriding procedure Finalize   (This : in out Controler) is
   begin
      This.Controled.Finalize;
   end Finalize;

   procedure Send_Socket
     (Socket :     GNAT.Sockets.Socket_Type;
      Item   :     Memory_Stream;
      Last   : out Ada.Streams.Stream_Element_Offset;
      To     :     access GNAT.Sockets.Sock_Addr_Type;
      Flags  :     GNAT.Sockets.Request_Flag_Type := GNAT.Sockets.No_Request_Flag) is
   begin
      GNAT.Sockets.Send_Socket
        (Socket => Socket,
         Item   => Item.Buffer.As_Pointer.all (Item.Buffer.As_Pointer.all'First .. Item.Cursor - 1),
         Last   => Last,
         To     => To,
         Flags  => Flags);
   end Send_Socket;

   function Image (Item : Ada.Streams.Stream_Element; Full : Boolean := False) return String;
   function Image (Item : Ada.Streams.Stream_Element; Full : Boolean := False) return String is
      Map : constant array (Stream_Element'(0) .. Stream_Element'(15)) of Character := "0123456789ABCDEF";
      Val : constant String := Map (Item / 16) & Map (Item mod 16);
   begin
      if Full then
         return "16#" & Val & "#";
      else
         return Val;
      end if;
   end Image;

   procedure Dump
     (This        : Memory_Stream;
      To          : not null access Ada.Streams.Root_Stream_Type'Class;
      Full_Buffer : Boolean := False) is
      First_Char : Boolean := True;
   begin
      for C of This.Buffer.As_Pointer.all (This.Buffer.As_Pointer.all'First .. This.Cursor - 1) loop
         if not First_Char then
            Character'Write (To, ' ');
            First_Char := False;
         end if;
         String'Write (To, Image (C));
      end loop;
   end Dump;

   function As_Standard_String
     (This : Memory_Stream;
      Full_Buffer : Boolean := False) return String is
   begin
      if Full_Buffer then
         return This.Buffer.As_String_Access.all (1 .. Positive (This.Get_Length));
      else
         return This.Buffer.As_String_Access.all (1 .. Positive (This.Pos));
      end if;
   end As_Standard_String;

   procedure As_Source
     (This        : Memory_Stream;
      To          : not null access Ada.Streams.Root_Stream_Type'Class;
      Full_Buffer : Boolean := False)   is
      Data : Stream_Element_Array renames This.Buffer.As_Pointer.all (0 .. (if Full_Buffer then This.Get_Length - 1 else This.Pos - 1));
      Pos : Natural := 0;
   begin
      String'Write (To, "(");
      for C of Data loop
         if Pos > 0 then
            Character'Write (To, ',');
            if Pos mod 16 = 0 then
               Character'Write (To, ASCII.LF);
            else
               Character'Write (To, ' ');
            end if;
         end if;
         Pos := Pos + 1;
         String'Write (To, Image (C, True));
      end loop;
      String'Write (To, ");");
   end As_Source;

end Stream_Tools.Memory_Streams;
