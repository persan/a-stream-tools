with Ada.Streams;
package Stream_Tools.Bufferd_Streams is
   type Bufferd_Stream (Size : Ada.Streams.Stream_Element_Offset)
     is new Ada.Streams.Root_Stream_Type with private;

   overriding procedure Read
     (Stream : in out Bufferd_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Bufferd_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

private
   use type Ada.Streams.Stream_Element_Count;
   subtype My_Stream_Element_Count is Ada.Streams.Stream_Element_Count range 1 .. 2 ** 16;
   protected type Buffer_Type (Size : Ada.Streams.Stream_Element_Offset) is
      entry Read
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset);
      entry Write
        (Item   : Ada.Streams.Stream_Element_Array);
   private
      procedure H_Read
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset);
      procedure H_Write
        (Item   : Ada.Streams.Stream_Element_Array);

      entry I_Read (My_Stream_Element_Count)
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset);
      entry I_Write (My_Stream_Element_Count)
        (Item   : Ada.Streams.Stream_Element_Array);

      Buffer         : Ada.Streams.Stream_Element_Array (1 .. Size);
      Read_Cursor    : Ada.Streams.Stream_Element_Offset := 1;
      Write_Cursor   : Ada.Streams.Stream_Element_Offset := 1;
      Count          : Ada.Streams.Stream_Element_Offset := 0;
   end Buffer_Type

   type Bufferd_Stream (Size : Ada.Streams.Stream_Element_Offset)
     is  new Ada.Streams.Root_Stream_Type with record
      Buffer : Buffer_Type (Size);
   end record;
end Stream_Tools.Bufferd_Streams;
