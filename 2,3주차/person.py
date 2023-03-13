# -*- coding: utf-8 -*-
"""Person

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1x7GK5-3vL15b5FupBoupjUAxw_rhyct7
"""

class Person :
  """Super Class"""
  total_count = 0                # 클래스 변수 : 클래스에서 공유되는 변수

  def __init__(self) :           # 생성자 메서드
    self.name = "홍길동"
    self.age = 3
    Person.total_count += 1


  def introduce(self) :          # 메서드
    print("제 이름은 {}이고, 나이는 {}살 입니다.".format(self.name, self.age))