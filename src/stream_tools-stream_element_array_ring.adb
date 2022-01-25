pragma Ada_2012;
with Ada.Unchecked_Conversion;
with System;
package body Stream_Tools.Stream_Element_Array_Ring is
   type Stream_Element_Offset_Access is access all Ada.Streams.Stream_Element_Offset with Storage_Size => 0;
   function As_Stream_Element_Offset_Access is new Ada.Unchecked_Conversion (System.Address, Stream_Element_Offset_Access);

   ------------------
   -- First_Cursor --
   ------------------

   function First_Cursor (Cont : Ring_Buffer) return Cursor is
      pragma Unreferenced (Cont);
   begin
      return raise Program_Error with "Unimplemented function First_Cursor";
   end First_Cursor;

   -------------
   -- Advance --
   -------------

   function Advance (Cont : Ring_Buffer; Position : Cursor) return Cursor is
   begin
      return raise Program_Error with "Unimplemented function Advance";
   end Advance;

   ------------------------
   -- Cursor_Has_Element --
   ------------------------

   function Cursor_Has_Element
     (Cont : Ring_Buffer; Position : Cursor) return Boolean
   is
   begin
      return
        raise Program_Error with "Unimplemented function Cursor_Has_Element";
   end Cursor_Has_Element;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element
     (Cont : Ring_Buffer; Position : Cursor) return Stream_Element_Array
   is
   begin
      return raise Program_Error with "Unimplemented function Get_Element";
   end Get_Element;
   ------------
   -- Output --
   ------------

   procedure Output (S : in out Ring_Buffer; Item : Stream_Element_Array) is
      function Free_Space return Stream_Element_Offset;
      function Free_Space return Stream_Element_Offset is
      begin

         return (if S.Head > S.Tail then
                    S.Buffer'Length - (S.Head - S.Tail)
                 elsif S.Head = S.Tail then
                    S.Buffer'Length
                 else
                    S.Tail - S.Head);
      end Free_Space;
      Size : Stream_Element_Offset;
   begin

      --  Validate that write is possible.
      if Item'Length + 4 > S.Buffer'Length then
         raise Constraint_Error with "oversized object " & Stream_Element_Offset'(Item'Length + 4)'Img;
      end if;

      --  Check free space.
      while Free_Space < Item'Length + 4 loop
         Size := As_Stream_Element_Offset_Access (S.Buffer (S.Tail)'Address).all;
         S.Tail := (S.Tail + Size + (Stream_Element_Offset'Size / Stream_Element'Size)) mod (S.Buffer'Length + 1);
      end loop;
      --  Object_Size := As_Stream_Element_Offset_Access(
      --  Finally write the data

   end Output;

   -----------
   -- Input --
   -----------

   function Input (S : in out Ring_Buffer) return Stream_Element_Array is
      pragma Unreferenced (S);
   begin

      pragma Compile_Time_Warning (Standard.True, "Input unimplemented");
      return raise Program_Error with "Unimplemented function Input";
   end Input;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (S : Ring_Buffer) return Boolean is
   begin
      return S.Tail = S.Head;
   end End_Of_File;

   -----------
   -- Reset --
   -----------

   procedure Reset (S : in out Ring_Buffer; Hard : Boolean := False) is
      pragma Unreferenced (S);
   begin
      pragma Compile_Time_Warning (Standard.True, "Reset unimplemented");
      raise Program_Error with "Unimplemented procedure Reset";
   end Reset;

   overriding procedure Read
     (Stream : in out Ring_Buffer;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
      pragma Unreferenced (Last);
   begin

      Stream.Tail := (Stream.Tail + Item'Length) mod Stream.Buffer'Length;
   end Read;

   overriding procedure Write
     (Stream : in out Ring_Buffer;
      Item   : Stream_Element_Array)  is
      Remaining : Stream_Element_Offset;
   begin
      if Item'Length < Stream.Buffer'Last - Stream.Head -- now wrap around
      then
         Stream.Buffer (Stream.Head .. Item'Length - 1) := Item;
      else
         Remaining := Stream.Buffer'Last - Stream.Head;
         Stream.Buffer (Stream.Head .. Stream.Buffer'Last) := Item (Item'First .. Item'First + Remaining);
         --  Stream.Buffer (Stream.Buffer'First .. Item (Item'First + (Stream.Buffer'Last - Stream.Head)));
      end if;
      Stream.Head := (Stream.Head + Item'Length) mod Stream.Buffer'Length;
   end Write;
end Stream_Tools.Stream_Element_Array_Ring;
