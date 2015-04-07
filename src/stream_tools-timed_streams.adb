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

with Ada.Environment_Variables;
with GNAT.Sockets;
with GNAT.Time_Stamp;
with GNAT.Formatted_String;
with GNAT.Regpat;
package body Stream_Tools.Timed_Streams is
   use Ada.Real_Time;
   use Ada.Streams;
   use Ada.Text_IO;
   use Event_Vectors;
   ------------
   -- Create --
   ------------

   not overriding procedure Create
     (Stream      : in out Timed_Stream;
      Path        : String;
      With_Header : Boolean := True)
   is
   begin
      Stream.With_Header := With_Header;
      Stream.Start_Time := Ada.Real_Time.Clock;
      Create (Stream.Target, Out_File, Path);
      if Stream.With_Header then
         Stream.Put_Header;
      end if;
      Stream.Start_Time := Clock;
      Stream.Mode := Out_File;
   end Create;

   not overriding procedure Open
     (Stream      : in out Timed_Stream;
      Path        : String;
      Ignore_EOF  : Boolean := False)
   is
      use GNAT.Regpat;
      M       : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("([\d.]+);([\d\n]+)");
      procedure Read (Line : String);
      procedure Read (Line : String) is
         Matches : GNAT.Regpat.Match_Array (0 .. GNAT.Regpat.Paren_Count (M));
      begin
         GNAT.Regpat.Match (M, Line, Matches);
         if Matches (Matches'First) /= No_Match then
            Stream.Buffer.Append ((To_Time_Span (Duration'Value (Line (Matches (1).First .. Matches (1).Last))),
                                  Stream_Element'Value (Line (Matches (2).First .. Matches (2).Last))));
         end if;
      end Read;
   begin
      Stream.Buffer.Clear;
      Open (Stream.Target, In_File, Path);
      while not Ada.Text_IO.End_Of_File (Stream.Target) loop
         Read (Get_Line (Stream.Target));
      end loop;
      Close (Stream.Target);
      Stream.C := Stream.Buffer.To_Cursor (1);
      Stream.Start_Time := Ada.Real_Time.Time_First;
      Stream.Mode := In_File;
      Stream.Null_EOF := Ignore_EOF;
   end Open;

   not overriding procedure Put_Line
     (Stream  : in out Timed_Stream;
      Item    : String) is
   begin
      Stream.Write;
      Put_Line (Stream.Target, Item);
   end Put_Line;

   not overriding procedure New_Line
     (Stream  : in out Timed_Stream;
      Spacing : Ada.Text_IO.Positive_Count := 1) is
   begin
      Stream.Write;
      New_Line (Stream.Target, Spacing);
   end New_Line;

   -----------
   -- Close --
   -----------

   not overriding procedure Close
     (Stream  : in out Timed_Stream)
   is
   begin
      if Stream.Mode = Out_File  and then Is_Open (Stream.Target) then
         Stream.Write;
         if Stream.With_Header then
            Stream.Put_Footer;
         end if;
         Close (Stream.Target);
      end if;
   end Close;

   -----------
   -- Write --
   -----------
   not overriding procedure Write
     (Stream : in out Timed_Stream;
      Item   : Ada.Streams.Stream_Element) is
   begin
      Stream.Buffer.Append ((Ada.Real_Time.Clock - Stream.Start_Time, Item));
   end Write;

   not overriding procedure Read
     (Stream : in out Timed_Stream;
      Item   : out Ada.Streams.Stream_Element) is
   begin
      if Stream.Start_Time = Ada.Real_Time.Time_First then
         Stream.Start_Time := Ada.Real_Time.Clock;
      end if;
      if Has_Element (Stream.C) then
         delay until Stream.Start_Time + Stream.Buffer (Stream.C).Time;
         Item := Stream.Buffer (Stream.C).Data;
         Stream.C := Next (Stream.C);
      elsif Stream.Null_EOF then
         Item := 0;
      else
         raise Constraint_Error with "Tried to read past last data in buffer";
      end if;
   end Read;

   not overriding procedure Write
     (Stream : in out Timed_Stream) is
      use GNAT.Formatted_String;
   begin
      for I of Stream.Buffer loop
         Put_Line (Stream.Target, -(+"%03f;%02d" & To_Duration (I.Time) & Integer (I.Data)));
      end loop;
      Stream.Buffer.Clear;
   end Write;

   not overriding procedure Put_Header
     (Stream  : in out Timed_Stream) is
   begin
      Stream.Put_Line ("-- -----------------------------------------------");
      Stream.Put_Line ("--  Sampling started : " & GNAT.Time_Stamp.Current_Time);
      Stream.Put_Line ("--    On host        : " & GNAT.Sockets.Host_Name);
      Stream.Put_Line ("--    By user        : " & Ada.Environment_Variables.Value ("USER"));
      Stream.Put_Line ("-- -----------------------------------------------");
   end Put_Header;

   not overriding procedure Put_Footer
     (Stream  : in out Timed_Stream) is
   begin         Stream.New_Line;
      Stream.Put_Line ("-- -----------------------------------------------");
      Stream.Put_Line ("--  Sampling Ended    : " & GNAT.Time_Stamp.Current_Time);
      Stream.Put_Line ("--  Sampling Duration :" & To_Duration (Clock - Stream.Start_Time)'Img);
      Stream.Put_Line ("-- -----------------------------------------------");

   end Put_Footer;

   overriding procedure Initialize (Object : in out Controler_Type) is
   begin
      null;
   end Initialize;

   overriding procedure Finalize   (Object : in out Controler_Type) is
   begin
      Object.Controled.Close;
   end Finalize;

end Stream_Tools.Timed_Streams;
