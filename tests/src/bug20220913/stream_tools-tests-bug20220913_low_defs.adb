package body Stream_Tools.Tests.BUG20220913_Low_Defs is

   use type Ada.Streams.Stream_Element_Offset;

   procedure Read
     (S    :     not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out Jp_List_Distribution_Acknowledge_T)
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Item'Size / Ada.Streams.Stream_Element'Size) with
        Import => True,
        Address => Item'Address;
   begin
      Ada.Streams.Stream_Element_Array'Read (S, Buffer);
   end Read;

   procedure Write
     (S    :    not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Jp_List_Distribution_Acknowledge_T)
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Item'Size / Ada.Streams.Stream_Element'Size) with
        Import => True,
        Address => Item'Address;
   begin
      Ada.Streams.Stream_Element_Array'Write (S, Buffer);
   end Write;

end Stream_Tools.Tests.BUG20220913_Low_Defs;
