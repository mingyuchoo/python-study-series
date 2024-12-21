def solution(data: str) -> str:
    """
    Run-Length Encoding
    """
    encoded: str = ''
    for i in range(0, len(data)):
        if encoded == '':
            encoded = data[i] + str(1)
        elif encoded[-2] == data[i]:
            encoded = encoded[0:-1] + str(int(encoded[-1]) + 1)
        else:
            encoded += data[i] + str(1)
    return encoded


if __name__ == "__main__":
    data:str = 'aaaaabbbccccccddddddddd'
    print(f'solution: {solution(data)}') # a5b3c6d9

    data = 'a'
    print(f'solution: {solution(data)}') # a1
