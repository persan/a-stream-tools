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

package body Stream_Tools.Pipes is

   use Ada.Streams;
   use Ada.Task_Identification;
   -----------
   -- Image --
   -----------

   function Image (Item : Ada.Streams.Stream_Element_Array) return String is
      Map    : constant array (Stream_Element (0) .. Stream_Element (15)) of Character := "0123456789ABCDEF";
      Cursor : Natural;
   begin
      return Ret : String (1 .. (Item'Length * 3) - 1) do
         Cursor := Ret'First;
         for I of Item loop
            if Cursor /= Ret'First then
               Ret (Cursor) :=  ' ';
               Cursor := Cursor + 1;
            end if;
            Ret (Cursor) := Map (I / 16);
            Cursor := Cursor + 1;
            Ret (Cursor) := Map (I mod 16);
            Cursor := Cursor + 1;
         end loop;
      end return;
   end Image;

   ----------
   -- Read --
   ----------
   overriding procedure Read
     (Stream : in out Pipe;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      Stream.Impl.Read (Item   => Item,
                        Last   => Last,
                        Caller => Ada.Task_Identification.Current_Task);
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Pipe;
      Item   : Ada.Streams.Stream_Element_Array)
   is
   begin
      Stream.Impl.Write (Item   => Item,
                         Caller => Ada.Task_Identification.Current_Task);
   end Write;

   -------------------
   -- Internal_Pipe --
   -------------------

   protected body Internal_Pipe is

      ----------
      -- Read --
      ----------

      entry Read
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset;
         Caller : Ada.Task_Identification.Task_Id)
        when Is_Closed or (Count > 0  and Data_Written)
      is
         Split_1 : Ada.Streams.Stream_Element_Offset;
         Split_2 : Ada.Streams.Stream_Element_Offset;
      begin
         if Is_Closed and Count = 0 then
            Last := Item'First - 1;
            return;
         elsif Reader_Task = Ada.Task_Identification.Null_Task_Id then
            Reader_Task := Caller;
         elsif Reader_Task /= Caller then
            raise Constraint_Error with "Multiple reader tasks not allowed";
         end if;

         Data_Read := True;
         if Item'Length > Capacity / 2 then
            raise Constraint_Error with "Not item to big";
         elsif Item'Last - Item'First >  Count then
            if Is_Closed then
               Last := Item'First - 1;
               Count := 0;
               return;
            else
               Data_Written := False;
               requeue Read;
            end if;
         elsif Out_Cursor + Item'Last - Item'First > Capacity then
            Split_1 := ((Out_Cursor + Item'Last - Item'First) mod Capacity);
            Split_2 := Item'Length - Split_1;
            Item (Item'First .. Item'First + Split_2 - 1) := Buffer (Out_Cursor .. Capacity);
            Item (Item'Last - Split_1 .. Item'Last) := Buffer (Buffer'First .. Buffer'First + Split_1);
         else
            Item := Buffer (Out_Cursor .. Out_Cursor + Item'Length - 1);
         end if;
         Out_Cursor := ((Out_Cursor + Item'Last - Item'First) mod Capacity) + 1;
         Last := Item'Last;
         Count := Count - Item'Length;
      end Read;

      -----------
      -- Write --
      -----------

      entry Write
        (Item   : Ada.Streams.Stream_Element_Array;
         Caller : Ada.Task_Identification.Task_Id)
        when Is_Closed or (Count < Capacity and Data_Read)
      is
         Split_1 : Ada.Streams.Stream_Element_Offset;
         Split_2 : Ada.Streams.Stream_Element_Offset;
      begin
         Data_Written := True;

         if Is_Closed then
            return;
         elsif Writer_Task = Ada.Task_Identification.Null_Task_Id then
            Writer_Task := Caller;
         elsif Writer_Task /= Caller then
            raise Constraint_Error with "Multiple reader tasks not allowed";
         end if;

         if Item'Length > Capacity / 2 then
            raise Constraint_Error with "Not room for item";
         elsif Item'Last - Item'First >= Capacity - Count then
            Data_Read := False;
            requeue Write;
         elsif In_Cursor + Item'Last - Item'First > Capacity then
            Split_1 := ((In_Cursor + Item'Last - Item'First) mod Capacity);
            Split_2 := Item'Length - Split_1;
            Buffer (In_Cursor .. Capacity) := Item (Item'First .. Item'First + Split_2 - 1);
            Buffer (Buffer'First .. Buffer'First + Split_1) := Item (Item'Last - Split_1 .. Item'Last);
         else
            Buffer (In_Cursor .. In_Cursor + Item'Length - 1) := Item;
         end if;

         In_Cursor := ((In_Cursor + Item'Last - Item'First) mod Capacity) + 1;
         Count := Count + Item'Length;
      end Write;

      ----------
      -- Dump --
      ----------
      procedure Dump (Put_Line : not null access procedure (Item : String)) is
      begin
         Put_Line ("Capacity   => " & Capacity'Img);
         Put_Line ("Buffer     => " & Image (Buffer));
         Put_Line ("Count      => " & Count'Img);
         Put_Line ("In_Cursor  => " & In_Cursor'Img);
         Put_Line ("Out_Cursor => " & Out_Cursor'Img);
      end Dump;

      procedure Close is
      begin
         Is_Closed := True;
      end Close;

      procedure Reset is
      begin
         Count        := 0;
         In_Cursor    := Buffer'First;
         Out_Cursor   := Buffer'First;
         Data_Written := True;
         Data_Read    := True;
         Is_Closed    := False;
         Reader_Task  := Ada.Task_Identification.Null_Task_Id;
         Writer_Task  := Ada.Task_Identification.Null_Task_Id;
      end Reset;

   end Internal_Pipe;

   ----------
   -- Dump --
   ----------
   procedure Dump (Stream : in out Pipe; Put_Line : not null access procedure (Item : String)) is
   begin
      Stream.Impl.Dump (Put_Line);
   end Dump;
   procedure Reset (Stream : in out Pipe) is
   begin
      Stream.Impl.Reset;
   end Reset;
   procedure Close (Stream : in out Pipe) is
   begin
      Stream.Impl.Close;
   end Close;

end Stream_Tools.Pipes;
