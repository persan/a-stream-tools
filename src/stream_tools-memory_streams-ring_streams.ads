package Stream_Tools.Memory_Streams.Ring_Streams is
   type Ring_Stream (Capacity : Ada.Streams.Stream_Element_Count) is new Memory_Stream with private;

   overriding procedure Read
     (Stream : in out Ring_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Ring_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

   function End_Of_File (Stream : in out Ring_Stream) return Boolean;

   procedure Reset (Stream : in out Ring_Stream; Hard : Boolean := False);

private
   type Ring_Stream (Capacity : Ada.Streams.Stream_Element_Count) is new Memory_Stream with record
      Buffer_Implementation : Ada.Streams.Stream_Element_Array (1 .. Capacity);
      Head                  : Ada.Streams.Stream_Element_Offset := 1;
      Tail                  : Ada.Streams.Stream_Element_Offset := 1;
   end record;
end Stream_Tools.Memory_Streams.Ring_Streams;
