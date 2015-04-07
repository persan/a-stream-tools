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

with GNAT.Formatted_String;
package body Stream_Tools.Bufferd_Streams is

   use Ada.Streams;
   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Bufferd_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
   begin
      if Item'Length > Stream.Size then
         raise Constraint_Error with "To large data" & Item'Length'Img & ">" &  Stream.Size'Img & ".";
      end if;
      Stream.Buffer.Write (Item);
   end Write;

   overriding procedure Read
     (Stream : in out Bufferd_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      if Item'Length > Stream.Size then
         raise Constraint_Error with "To large data" & Item'Length'Img & ">" &  Stream.Size'Img & ".";
      end if;
      Stream.Buffer.Read (Item, Last);
   end Read;

   -----------------
   -- Buffer_Type --
   -----------------

   protected body Buffer_Type is
   ----------
   -- Read --
   ----------
      entry Read
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset)
        when Count > 0
      is
      begin
         if Item'Length < Count then
            requeue I_Read (Item'Length);
         else
            H_Read (Item, Last);
         end if;
      end Read;

      -----------
      -- Write --
      -----------

      entry Write
        (Item   : Ada.Streams.Stream_Element_Array)
        when Count < Size
      is
      begin
         if Item'Length > Buffer'Length - Count then
            requeue I_Write (Item'Length);
         else
            H_Write (Item);
         end if;
      end Write;

      entry I_Read (for R_Count in My_Stream_Element_Count)
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset)
      when
        R_Count <= Count  is
      begin
         H_Read (Item, Last);
      end I_Read;

      entry I_Write (for W_Count in My_Stream_Element_Count)
        (Item   : Ada.Streams.Stream_Element_Array)
      when
        W_Count >= Size - Count is
      begin
         H_Write (Item);
      end I_Write;

      procedure H_Read
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset) is
      begin
         if Read_Cursor + Item'Length <= Size then
            Item := Buffer (Read_Cursor .. Read_Cursor + Item'Length - 1);
         else
            declare
               Split : constant Stream_Element_Offset := Buffer'Last - Read_Cursor;
            begin
               Item (Item'First .. Item'First + Split) := Buffer (Read_Cursor .. Buffer'Last);
               Item (Item'First + Split + 1 .. Item'Last) := Buffer (Buffer'First .. Item'Length - Split - 1);
            end;
         end if;
         Read_Cursor := Buffer'First + ((Read_Cursor + Item'Length - 1) mod Buffer'Length);
         Last := Item'Last;
         Count := Count - Item'Length;
      end H_Read;

      procedure H_Write
        (Item   : Ada.Streams.Stream_Element_Array) is
      begin
         if Write_Cursor + Item'Length <= Size then
            Buffer (Write_Cursor .. Write_Cursor + Item'Length - 1) := Item;
         else
            declare
               Split : constant Ada.Streams.Stream_Element_Offset := Buffer'Last - Write_Cursor;
            begin
               Buffer (Write_Cursor .. Buffer'Last) := Item (Item'First .. Item'First + Split);
               Buffer (Buffer'First .. Buffer'First + Split) := Item (Item'First + Split .. Item'Last);
            end;
         end if;
         Write_Cursor := Buffer'First + (Write_Cursor mod Buffer'Length);
         Count := Count + Item'Length;
      end H_Write;

      function Image return String is
         Ret_Buffer : String (1 .. 68 * (Integer (Count) / 16 + 1)) := (others => ' ');
         Cursor     : Positive := Ret_Buffer'First;
         procedure Append (S : String);
         procedure Append (S : String) is
         begin
            Ret_Buffer (Cursor .. Cursor + S'Length - 1) := S;
            Cursor := Cursor + S'Length;
         end Append;
         use GNAT.Formatted_String;
         C          : Ada.Streams.Stream_Element_Offset := Read_Cursor;
      begin
         for Ix in Stream_Element_Offset'(1) .. Count loop
            if (Ix - 1) mod 16 = 0 then
               Append (-(+"%08x:" & Integer (Ix - 1)));
            end if;

            Append (-(+" %02x" & Integer (Buffer (C))));
            C := C + 1;
            if C > Buffer'Last then
               C := Buffer'First;
            end if;
            if (Ix) mod 16 = 0 then
               Append (ASCII.LF & "");
            end if;
         end loop;
         return Ret_Buffer (Ret_Buffer'First .. Cursor);
      end Image;

      function GetCount return Ada.Streams.Stream_Element_Offset is
      begin
         return Count;
      end GetCount;

   end Buffer_Type;

   not overriding function Image
     (Stream : in out Bufferd_Stream) return String is
   begin
      return Stream.Buffer.Image;
   end Image;

   function GetCount
     (Stream : in out Bufferd_Stream) return Ada.Streams.Stream_Element_Offset is
   begin
      return Stream.Buffer.GetCount;
   end GetCount;

end Stream_Tools.Bufferd_Streams;
