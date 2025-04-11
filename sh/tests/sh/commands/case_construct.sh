case value in
    (value) echo correct;;
    (other) echo incorrect;;
esac

case value in
    value) echo correct;;
    other) echo incorrect
esac

case value in
    (*) echo correct
esac

case value in
    (something) echo incorrect;;
    (something_else | value) echo correct;;
esac

case value in
    (something | value) echo correct;;
    (other | value) echo incorrect;;
esac

case value in
  (value) echo correct;;
  ($(echo incorrect)) echo incorrect;;
esac