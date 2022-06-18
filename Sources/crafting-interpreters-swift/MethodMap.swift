func methodMapping(method: String, params: Any) -> Any? {
    switch method {
        case TokenEnum.OUTPUT.value: 
            outputMethod(value: params)
            break
        case TokenEnum.MAX.value:
            return maxMethod(value: params as! [Double])
        case TokenEnum.MIN.value:
            return minMethod(value: params as! [Double])
        case TokenEnum.AVG.value:
            return avgMethod(value: params as! [Double])
        case TokenEnum.SUM.value:
            return sumMethod(value: params as! [Double])
        default:
            print("no way hozay")
    }

    return nil
}

func outputMethod(value: Any) -> Void {
    print(value);
}

func maxMethod(value: [Double]) -> Double {
    return value.max()!;
}

func minMethod(value: [Double]) -> Double {
    return value.min()!;
}

func avgMethod(value: [Double]) -> Double {
    let sum = value.reduce(0,+)
    let length = value.count
    return sum / Double(length)
}

func sumMethod(value: [Double]) -> Double {
    return value.reduce(0,+)
}