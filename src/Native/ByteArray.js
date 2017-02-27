var _bgourlie$rs_nes_debugger_frontend$Native_ByteArray = function () {

var empty = new Uint8ClampedArray(0);

function fromBase64(base64)
{
    try
    {
        var arr = Uint8ClampedArray.from(atob(base64), c => c.charCodeAt(0));
        return _elm_lang$core$Result$Ok(arr);
    }
    catch (e)
    {
        return _elm_lang$core$Result$Err('The base64-encoded string must consist of ASCII-encoded characters in multiples of 4.')
    }
}

function get(i, arr)
{
    if (i < 0 || arr.byteLength <= i)
    {
        return _elm_lang$core$Maybe$Nothing;
    }

    return _elm_lang$core$Maybe$Just(arr[i]);
}

function slice(start, end, arr)
{
    return arr.slice(start, end);
}

function toList(arr)
{
    var list = _elm_lang$core$Native_List.Nil;

	for (var i = arr.byteLength - 1; i >= 0; i--)
	{
		list = _elm_lang$core$Native_List.Cons(arr[i], list)
	}

	return list;
}

return {
    fromBase64: fromBase64,
    empty: empty,
    get: F2(get),
    slice: F3(slice),
    toList: toList
};

}();