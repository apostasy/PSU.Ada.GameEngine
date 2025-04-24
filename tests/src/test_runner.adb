with Tests.Test_Camera;
use Tests;  -- Add this line to make the Test_Camera package directly accessible

procedure Test_Runner is
begin
   Test_Camera.Test;  -- Now this will work
end Test_Runner;