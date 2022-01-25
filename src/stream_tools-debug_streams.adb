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
