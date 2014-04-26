Config { font = "xft:Terminess\ Powerline:size=8"
       , bgColor = "#1c1c1c"
       , fgColor = "#9e9e9e"
       , position = BottomW C 100 
       , commands = [ -- Run Com "echo" ["$USER"] "username" 864000
		      Run Com "hostname" ["-s"] "hostname" 864000
		    -- , Run Com "uname" ["-sr"] "os" 864000
		    , Run Com "mem" ["-p"] "memused" 36000
		    , Run Com "loadavg" [] "loadavg" 10
		    -- , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                    -- , Run Com "inet" [] "inet" 864000
                    , Run Com "dskusg" [] "dskusg" 10
		    , Run Com "batt"   [] "batt" 10
		    , Run Com "strpdt" [] "strpdt" 864000
		    , Run Com "binclk" [] "binclk" 1
                    , Run StdinReader
                    ]
       , sepChar = "'"
       , alignSep = "}{"
       , template = " 'StdinReader' }{ 'dskusg' | 'hostname' | mem: 'memused'% | loadavg: 'loadavg' | batt: 'batt' | 'strpdt' | <fc=#ff005f>'binclk'</fc> "
}

