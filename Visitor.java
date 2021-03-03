/*

data MyList a = EmptyList | ConsList a MyList

rem :: (Eq a) => a -> MyList a -> MyList a
rem a EmptyList = EmptyList
rem a (ConsList b l) = if a == b then rem a l
                                 else ConsList b (rem a l)

subst :: (Eq a) => a -> MyList a -> a  -> MyList a
subst a EmptyList n = EmptyList
subst a (ConsList b l) n = if a == b then ConsList n (subst a l n)
                                     else ConsList b (subst a l n)

 */

interface ListVisitorI {
    MyList forEmpty();

    MyList forCons(Object o, MyList l);
}

abstract class MyList {
    abstract MyList accept(ListVisitorI lvFn);
}

class EmptyList extends MyList {

    public MyList accept(ListVisitorI lvFn) {
        return lvFn.forEmpty();
    }

    public String toString() {
        return "new " + getClass().getName();
    }
}

class ConsList extends MyList {
    private Object o;
    private MyList l;

    ConsList(Object _o, MyList _l) {
        l = _l;
        o = _o;
    }

    public MyList accept(ListVisitorI lvFn) {
        return lvFn.forCons(o, l);
    }

    public String toString() {
        return "new " + getClass().getName() + "(" + o + ", " + l + ")";
    }
}

class RemV implements ListVisitorI {
    private Object _o;

    RemV(Object o) {
        _o = o;
    }

    public MyList forEmpty() {
        return new EmptyList();
    }

    public MyList forCons(Object o, MyList l) {
        if (o.equals(_o)) {
            return l.accept(this);
        }
        return new ConsList(o, l.accept(this));
    }
}

class SubstV implements ListVisitorI {
    private Object n, a;

    SubstV(Object _n, Object _a) {
        n = _n;
        a = _a;
    }

    public MyList forEmpty() {
        return new EmptyList();
    }

    public MyList forCons(Object o, MyList l) {
        if (o.equals(a)) return new ConsList(n, l.accept(this));
        return new ConsList(o, l.accept(this));
    }

}

class AddBeforEmpty implements ListVisitorI {
    public MyList forEmpty() {
        return new ConsList(0, new EmptyList());
    }

    public MyList forCons(Object o, MyList l) {
        return new ConsList(o, l.accept(this));
    }
}

class Visitor {
    public static void main(String[] args) {
        MyList l = new ConsList(1, new EmptyList());
        MyList r = new ConsList(2, l);
        MyList t = new ConsList(3, r);
        SubstV s = new SubstV(4, 2);
        System.out.println(t.accept(s));
        System.out.println(t.accept(new RemV(2)));
        System.out.println(t.accept(new AddBeforEmpty()));
    }
}



