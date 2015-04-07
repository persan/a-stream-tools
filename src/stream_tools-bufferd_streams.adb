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

package body Stream_Tools.Bufferd_Streams is

   use Ada.Streams;
   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Bufferd_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
   begin
      if Item'Length > Stream.Capacity then
         raise Constraint_Error with "To large data" & Item'Length'Img & ">" &  Stream.Capacity'Img & ".";
      end if;
      for I of Item loop
         Stream.Buffer.Insert (I);
      end loop;
   end Write;

   overriding procedure Read
     (Stream : in out Bufferd_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
      pragma Unreferenced (Last);
   begin
      if Item'Length > Stream.Capacity then
         raise Constraint_Error with "To large data" & Item'Length'Img & ">" &  Stream.Capacity'Img & ".";
      end if;
      for I of Item loop
         Stream.Buffer.Remove (I);
      end loop;
      Last := Item'Last;
   end Read;

end Stream_Tools.Bufferd_Streams;
