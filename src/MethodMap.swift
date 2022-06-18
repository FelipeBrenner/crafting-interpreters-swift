func outputMethod(value: Any) -> Void {
    print(value);
}

func maxMethod(value: [Double]) -> Double {
    return value.max();
}

func minMethod(value: [Double]) -> Double {
    return value.min();
}

func avgMethod(value: [Double]) -> Double {
    var sum = value.reduce(0,+)
    var length = value.count
    return sum / length
}
func sumMethod(value: [Double]) -> Double {
    return value.reduce(0,+)
}

var methodMap: [(key: String, method: (Any) -> Any)] = [
    (key: TokenEnum.OUTPUT.value, method: outputMethod),
    (key: TokenEnum.MAX.value, method: maxMethod),
    (key: TokenEnum.MIN.value, method: minMethod),
    (key: TokenEnum.AVG.value, method: avgMethod),
    (key: TokenEnum.SUM.value, method: sumMethod)
]
