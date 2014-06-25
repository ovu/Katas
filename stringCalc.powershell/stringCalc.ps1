function AddNumbers ([string] $numbers) {
    If (!$numbers){
        return 0;
    } Else {
        $numbers.Split(",") | foreach -begin {$sum = 0} -process {$sum += $_} 
        return $sum;
    }
}