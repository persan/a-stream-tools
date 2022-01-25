with Ada.Streams;
with Stream_Tools.Memory_Streams;
package Stream_Tools.Stream_Element_Array_Ring is
   use Ada.Streams;
   type Cursor (<>) is private;

   type Ring_Buffer (Capacity : Ada.Streams.Stream_Element_Count) is new Stream_Tools.Memory_Streams.Memory_Stream with private
   with
     Iterable => (First        => First_Cursor,
                  Next         => Advance,
                  Has_Element  => Cursor_Has_Element,
                  Element      => Get_Element);
   --
   function First_Cursor (Cont : Ring_Buffer) return Cursor;
   function Advance (Cont : Ring_Buffer; Position : Cursor) return Cursor;
   function Cursor_Has_Element (Cont : Ring_Buffer; Position : Cursor) return Boolean;
   function Get_Element (Cont : Ring_Buffer; Position : Cursor) return Stream_Element_Array;
   --  Note that the iterator pops the Items from the back of the Queue as well.

   procedure Output (S : in out Ring_Buffer; Item : Stream_Element_Array);
   --  Write the Item to the buffer and pops tha last(s) until there is room for the object.
   --  raises constraint_Error and clears the buffer if the buffer is'nt large enogh to contain the item.

   function Input (S : in out Ring_Buffer) return Stream_Element_Array;
   --  Reads the last element in the buffer and popps the buffer

   function End_Of_File (S : Ring_Buffer) return Boolean;
   --  Returns true if there is no more Items left in the buffer.

   procedure Reset (S : in out  Ring_Buffer; Hard : Boolean := False);

private
   type Ring_Buffer (Capacity : Ada.Streams.Stream_Element_Count) is new Stream_Tools.Memory_Streams.Memory_Stream with record
      Buffer  : Ada.Streams.Stream_Element_Array (0 .. Capacity);
      Head    : Ada.Streams.Stream_Element_Offset := 0;
      Tail    : Ada.Streams.Stream_Element_Offset := 0;
   end record;

   overriding procedure Read
     (Stream : in out Ring_Buffer;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Ring_Buffer;
      Item   : Stream_Element_Array);

   type Cursor (C : not null access Ring_Buffer) is record
      null;
   end record;

end Stream_Tools.Stream_Element_Array_Ring;
