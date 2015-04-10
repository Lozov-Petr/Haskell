{
    read(n);
    index = 2;
    fact = 1;
    while index <= n do
    {
        fact = fact * index;
        index = index + 1;
    }
    write(fact);
}