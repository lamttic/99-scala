import org.specs2.mutable.Specification

class WorkingWithListSpec extends Specification {
  val emptyList = List()
  val oneSizeList = List(1)
  val commonList = List(1, 1, 2, 3, 5, 8)
  val palindromeList = List(1, 2, 3, 2, 1)
  val nestedList = List(List(1, 1), 2, List(3, List(5, 8)))
  val duplicateList =
    List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  val stringList =
    List('a', 'b', 'c', 'c', 'd')
  val longStringList =
    List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')

  "P01 is the problem of getting the last element" >> {
    "success" >> {
      WorkingWithList.last(commonList) mustEqual Some(8)
    }
    "none" >> {
      WorkingWithList.last(emptyList) mustEqual None
    }
  }

  "P02 is the problem of getting the n-1th element" >> {
    "success" >> {
      WorkingWithList.penultimate(commonList) mustEqual Some(5)
    }
    "none" >> {
      "because of empty list" >> {
        WorkingWithList.penultimate(emptyList) mustEqual None
      }
      "because of unsufficient size" >> {
        WorkingWithList.penultimate(oneSizeList) mustEqual None
      }
    }
  }

  "P03 is the problem of getting the kth element" >> {
    "success" >> {
      WorkingWithList.nth(2, commonList) mustEqual Some(2)
    }
    "none" >> {
      "because of empty list" >> {
        WorkingWithList.nth(0, emptyList) mustEqual None
      }
      "because of k is too large" >> {
        WorkingWithList.nth(10, commonList) mustEqual None
      }
    }
  }

  "P04 is the problem of getting element size" >> {
    "success" >> {
      "empty list" >> {
        WorkingWithList.length(emptyList) mustEqual 0
      }
      "common case" >> {
        WorkingWithList.length(commonList) mustEqual 6
      }
    }
  }

  "P05 is the problem of converting a given list in reverse order" >> {
    "success" >> {
      "empty list" >> {
        WorkingWithList.reverse(emptyList) mustEqual List()
      }
      "common case" >> {
        WorkingWithList.reverse(commonList) mustEqual commonList.reverse
      }
    }
  }

  "P06 is the problem of checking palindrome" >> {
    "success" >> {
      "common case" >> {
        WorkingWithList.isPalindrome(palindromeList) mustEqual true
      }
    }
  }

  "P07 is the problem of converting flatten list" >> {
    "success" >> {
      "common case" >> {
        WorkingWithList.flatten(nestedList) mustEqual commonList
      }
    }
  }

  "P08 is the problem that eliminates consecutive duplicates" >> {
    "success" >> {
      "common case" >> {
        WorkingWithList.compress(duplicateList) mustEqual List('a', 'b', 'c',
          'a', 'd', 'e')
      }
    }
  }

  "P09 is the problem of packing consecutive duplicates" >> {
    "success" >> {
      "common case" >> {
        WorkingWithList.pack(duplicateList) mustEqual List(
          List('a', 'a', 'a', 'a'),
          List('b'),
          List('c', 'c'),
          List('a', 'a'),
          List('d'),
          List('e', 'e', 'e', 'e')
        )
      }
    }
  }

  "P10 is the problem of encoding run-length" >> {
    "success" >> {
      "common case" >> {
        WorkingWithList.encode(duplicateList) mustEqual List(
          (4, 'a'),
          (1, 'b'),
          (2, 'c'),
          (2, 'a'),
          (1, 'd'),
          (4, 'e')
        )
      }
    }
  }

  "P11 is the problem of getting modified run-length encoding" >> {
    "success" >> {
      "common case" >> {
        WorkingWithList.encodeModified(duplicateList) mustEqual List(
          (4, 'a'),
          'b',
          (2, 'c'),
          (2, 'a'),
          'd',
          (4, 'e')
        )
      }
    }
  }

  "P12 is the problem that decode a run-length encoded list" >> {
    "success" >> {
      "common case" >> {
        WorkingWithList.decode(WorkingWithList.encode(duplicateList)) mustEqual duplicateList
      }
    }
  }

  "P13 is the problem of encoding run-length directly" >> {
    "success" >> {
      "common case" >> {
        WorkingWithList.encodeDirect(duplicateList) mustEqual List(
          (4, 'a'),
          (1, 'b'),
          (2, 'c'),
          (2, 'a'),
          (1, 'd'),
          (4, 'e')
        )
      }
    }
  }

  "P14 is the problem of making duplicate element list" >> {
    "success" >> {
      "common case" >> {
        WorkingWithList.duplicate(stringList) mustEqual List('a', 'a', 'b', 'b',
          'c', 'c', 'c', 'c', 'd', 'd')
      }
    }
  }

  "P15 is the problem of making n-times element list" >> {
    "success" >> {
      "common case" >> {
        WorkingWithList.duplicateN(3, stringList) mustEqual List('a', 'a', 'a',
          'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd')
      }
    }
  }

  "P16 is the problem that drop nth element from list" >> {
    "success" >> {
      "common case" >> {
        WorkingWithList.drop(3, longStringList) mustEqual List('a', 'b', 'c',
          'e', 'f', 'g', 'h', 'i', 'j', 'k')
      }
      "nothing changed" >> {
        "because of too large idx" >> {
          WorkingWithList.drop(10, stringList) mustEqual stringList
        }
      }
    }
  }
}
