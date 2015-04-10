{
    array = {elem = [], length = 0};
    while !EOF do
    {
    	read(array.elem[array.length]);
    	array.length = array.length + 1
    };
    i = 0;
    while i < array.length - 1 do
    {
        j = i + 1;
        while j < array.length do
        {
            if array.elem[i] > array.elem[j] then
            {
                 temp = array.elem[i];
                 array.elem[i] = array.elem[j];
                 array.elem[j] = temp
            };
            j = j + 1
        };
        i = i + 1
    };
    i = 0;
    while i < array.length do
    {
    	write(array.elem[i]);
    	i = i + 1
    }
}