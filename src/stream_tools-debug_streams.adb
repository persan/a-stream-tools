package body Stream_Tools.Debug_Streams is
   pragma Warnings (Off);
   ------------
   -- Create --
   ------------

   not overriding procedure Create
     (Stream      : in out Debug_Stream;
      Path        : String)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      raise Program_Error with "Unimplemented procedure Create";
   end Create;

   ----------
   -- Open --
   ----------

   not overriding procedure Open
     (Stream      : in out Debug_Stream;
      Path        : String)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Open unimplemented");
      raise Program_Error with "Unimplemented procedure Open";
   end Open;

   --------------
   -- Put_Line --
   --------------

   not overriding procedure Put_Line
     (Stream  : in out Debug_Stream;
      Item    : String)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Put_Line unimplemented");
      raise Program_Error with "Unimplemented procedure Put_Line";
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   not overriding procedure New_Line
     (Stream  : in out Debug_Stream;
      Spacing : Ada.Text_IO.Positive_Count := 1)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "New_Line unimplemented");
      raise Program_Error with "Unimplemented procedure New_Line";
   end New_Line;

   -----------
   -- Close --
   -----------

   not overriding procedure Close
     (Stream  : in out Debug_Stream)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Close unimplemented");
      raise Program_Error with "Unimplemented procedure Close";
   end Close;

   -----------
   -- Write --
   -----------

   not overriding procedure Write
     (Stream : in out Debug_Stream;
      Item   : Ada.Streams.Stream_Element)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

   ----------
   -- Read --
   ----------

   not overriding procedure Read
     (Stream : in out Debug_Stream;
      Item   : out Ada.Streams.Stream_Element)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      raise Program_Error with "Unimplemented procedure Read";
   end Read;

   ----------------
   -- Put_Header --
   ----------------

   not overriding procedure Put_Header
     (Stream  : in out Debug_Stream)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Put_Header unimplemented");
      raise Program_Error with "Unimplemented procedure Put_Header";
   end Put_Header;

   ----------------
   -- Put_Footer --
   ----------------

   not overriding procedure Put_Footer
     (Stream  : in out Debug_Stream)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Put_Footer unimplemented");
      raise Program_Error with "Unimplemented procedure Put_Footer";
   end Put_Footer;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Debug_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      raise Program_Error with "Unimplemented procedure Read";
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Debug_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

end Stream_Tools.Debug_Streams;
