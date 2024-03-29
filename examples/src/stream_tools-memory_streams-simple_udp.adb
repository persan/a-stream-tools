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

with Stream_Tools.Timed_Streams.Output;
procedure Stream_Tools.Memory_Streams.Simple_UDP is
   Real_Buffer : String (1 .. 1024) := [others => '#'];
   S           : aliased Stream_Tools.Memory_Streams.Memory_Stream;
   Outf        : aliased Stream_Tools.Timed_Streams.Output.Timed_Output_Stream;
begin
   Outf.Create ("test.txt");
   S.Set_Address (Real_Buffer'Address);
   S.Set_Length (Real_Buffer'Length);

   String'Write (S'Access, "Funn");
   Integer'Write (S'Access, 123);

   Memory_Streams.Memory_Stream'Write (Outf'Access, S);
   Outf.Close;
end Stream_Tools.Memory_Streams.Simple_UDP;
