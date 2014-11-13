feature A is
do
    a := * 3 3
	print ( a )

	b := / 3 3
	print ( b )

	c := - 3 3
	print ( c )

	d := + 3 3
	print ( d )

	if > 4 3 then
	    print ( 0 )
	else
	    print ( 1 )
	end

    if >= 4 3 then
        print ( 0 )
    else
        print ( 1 )
    end

    if >= 3 3 then
        print ( 0 )
    else
        print ( 1 )
    end

	if < 4 3 then
	    print ( 0 )
    else
        print ( 1 )
    end

    if <= 4 3 then
        print ( 0 )
    else
        print ( 1 )
    end

    if <= 3 3 then
        print ( 0 )
    else
        print ( 1 )
    end

    if /= 3 3 then
        print ( 0 )
    else
        print ( 1 )
    end

    if /= 4 3 then
        print ( 0 )
    else
        print ( 1 )
    end

    from
        y := 1
    until
        >= y 10
    loop
        y := + y 1
        print ( y )
    end

    from
        x := 10
    until
        <= x 1
    loop
        x := - x 1
        print ( x )
    end

    from
        z := 100
    until
        = z 1
    loop
        z := / z 10
        print ( z )
    end

    from
        w := 1
    until
        = w 128
    loop
        w := * w 2
        print ( w )
    end

    v := 1
    u := 2
    if = v 64 then
        print ( 0 )
        if >= u 2 then
            print ( 1 )
        else
            print ( 0 )
        end
    else
        print ( 0 )
    end

end