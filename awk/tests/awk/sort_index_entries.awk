# Sort index entries of the form \entry{KEY}{PAGE}{TEXT} by key, in the
# style of texinfo's texindex: every statement here is one its awk
# program depends on.

function del_array(a)
{
	split("", a)
}

# Fills the caller's unset local with the characters of `string`.
function char_split(string, array)
{
	return split(string, array, "")
}

function join(array, start, end, sep,	# parameters
	result, i)			# locals
{
	result = array[start]
	for (i = start + 1; i <= end; i++)
		result = result sep array[i]
	return result
}

# Splits `record` into the brace-delimited fields after its command.
function field_split(record, fields, start, end,
	chars, numchars, out, delim_count, i, j, k)
{
	del_array(fields)
	numchars = char_split(record, chars)
	i = index(record, start) + 1
	j = 1
	k = 1
	delim_count = 1
	for (; i <= numchars; i++) {
		if (chars[i] == start) {
			delim_count++
			out[k++] = chars[i]
		} else if (chars[i] == end) {
			delim_count--
			if (delim_count == 0) {
				fields[j++] = join(out, 1, k - 1, "")
				del_array(out)
				k = 1
				i++
				delim_count = 1
			} else
				out[k++] = chars[i]
		} else
			out[k++] = chars[i]
	}
	return j - 1
}

function initial_of(key,	nextchar)
{
	nextchar = substr(key, 1, 1)
	if (nextchar >= "a" &&
	    nextchar <= "z")
		return toupper(nextchar)
	return nextchar
}

{
	n = field_split($0, f, "{", "}")
	if (n != 3 ||
	    f[1] == "") {
		printf("bad entry: %s\n", $0)
		next
	}
	count++
	keys[count] = f[1]
	pages[count] = f[2]
	texts[count] = f[3]
}

END {
	# insertion sort, with an empty for-loop condition
	for (i = 2; ; i++) {
		if (i > count)
			break
		key = keys[i]; page = pages[i]; text = texts[i]
		for (j = i - 1; j >= 1 && keys[j] > key; j--) {
			keys[j + 1] = keys[j]; pages[j + 1] = pages[j]; texts[j + 1] = texts[j]
		}
		keys[j + 1] = key; pages[j + 1] = page; texts[j + 1] = text
	}
	for (i = 1; i <= count; i++) {
		initial = initial_of(keys[i])
		if (initial != previous)
			print "\\initial {" initial "}"
		previous = initial
		print "\\entry{" texts[i] "}{" pages[i] "}"
	}
}
