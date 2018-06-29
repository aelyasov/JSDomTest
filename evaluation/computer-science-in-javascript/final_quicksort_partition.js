/*t [int]:int:int  */
function test(items, left, right) {

    var pivot = items[Math.floor((right + left) / 2)],
        i = left,
        j = right;

    while (i <= j) {
        while (items[i] < pivot) {
            i++;
        }
        while (items[j] > pivot) {
            j--;
        }
        if (i <= j) {
            var temp = items[i];
            items[i] = items[j];
            items[j] = temp;

            i++;
            j--;
        }
    }
    return i;
}