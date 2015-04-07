package body Stream_Tools.Timed_Streams.Input is

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Timed_Input_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      for I of Item loop
         Stream.Read (I);
      end loop;
      Last := Item'Last;
   end Read;

   overriding procedure Write
     (Stream : in out Timed_Input_Stream;
      Item   : Ada.Streams.Stream_Element_Array) is
   begin
      raise Program_Error with "Write on Input_Streams not allowed";
   end Write;

   overriding procedure Create
     (Stream      : in out Timed_Input_Stream;
      Path        : String;
      With_Header : Boolean := True) is
   begin
      raise Program_Error with "Create on Input_Streams not allowed";
   end Create;

end Stream_Tools.Timed_Streams.Input;
