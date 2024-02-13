# Draw a triangle
# ----
# n = 5
# *
# **
# ***
# ****
# *****
def triangle(count):
    num = 1
    while num <= count:
        print("*" * num)
        num += 1

# Draw a upside down triangle
# ----
# n = 5
# *****
# ****
# ***
# **
# *
def upside_down_triangle(count):
    num = count
    while num >= 1:
        print("*" * num)
        num -= 1


# Draw a Diamond
# ----
# n = 5
#      *
#     ***
#    *****
#   *******
#  *********
# ***********
#  *********
#   *******
#    *****
#     ***
#      *
def diamond(count):
    number_of_space = count
    number_of_star = 0
    while number_of_space > 0:
        print(" " * number_of_space, "*" + "*" * number_of_star * 2)
        number_of_space -= 1
        number_of_star += 1

    while number_of_space <= count:
        print(" " * number_of_space, "*" + "*" * number_of_star * 2)
        number_of_space += 1
        number_of_star -= 1


if __name__ == "__main__":
    count = 5
    triangle(count)
    upside_down_triangle(count)
    diamond(count)
