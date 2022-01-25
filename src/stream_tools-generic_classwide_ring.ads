with Ada.Streams;
private with GNAT.Semaphores;
private with System;
private with Ada.Finalization;

generic
   type T (<>) is abstract tagged private;
package Stream_Tools.Generic_Classwide_Ring is

   type Cursor (<>) is private;

   type Ring_Buffer (Capacity : Ada.Streams.Stream_Element_Count;
                     Max_Serialized_Object_Size : Ada.Streams.Stream_Element_Count) is tagged  limited private with
     Iterable => (First        => First_Cursor,
                  Next         => Advance,
                  Has_Element  => Cursor_Has_Element,
                  Element      => Get_Element);

   function First_Cursor (Cont : Ring_Buffer) return Cursor;
   function Advance (Cont : Ring_Buffer; Position : Cursor) return Cursor;
   function Cursor_Has_Element (Cont : Ring_Buffer; Position : Cursor) return Boolean;
   function Get_Element (Cont : Ring_Buffer; Position : Cursor) return T'Class;
   --  Note that the iterator pops the Items from the back of the Queue as well.

   procedure Output (S : access Ring_Buffer; Item : T'Class);
   --  Write the Item to the buffer and pops tha last(s) until there is room for the object.
   --  raises constraint_Error and clears the buffer if the buffer is'nt large enogh to contain the item.

   function Input (S : access Ring_Buffer) return T'Class;
   --  Reads the last element in the buffer and popps the buffer

   function End_Of_File (S : access constant Ring_Buffer) return Boolean;
   --  Returns true if there is no more Items left in the buffer.

   procedure Reset (S : access  Ring_Buffer; Hard : Boolean := False);

private

   type Ring_Buffer (Capacity : Ada.Streams.Stream_Element_Count;
                     Max_Serialized_Object_Size : Ada.Streams.Stream_Element_Count) is
     new Ada.Streams.Root_Stream_Type with record
      Buffer  : Ada.Streams.Stream_Element_Array (0 .. Capacity);
      Temp    : Ada.Streams.Stream_Element_Array (0 .. Max_Serialized_Object_Size);
      Head    : Ada.Streams.Stream_Element_Offset := 0;
      Tail    : Ada.Streams.Stream_Element_Offset := 0;
      Counter : Ada.Streams.Stream_Element_Offset := 0;
      Lock    : aliased GNAT.Semaphores.Binary_Semaphore (True, System.Default_Priority);
   end record;

   overriding procedure Read
     (Stream : in out Ring_Buffer;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Ring_Buffer;
      Item   : Ada.Streams.Stream_Element_Array);

   type Cursor_Controll (C : not null access Ring_Buffer) is new Ada.Finalization.Controlled with null record;
   type Cursor (C : not null access Ring_Buffer) is record
      Controll : Cursor_Controll (C);
   end record;

end Stream_Tools.Generic_Classwide_Ring;
