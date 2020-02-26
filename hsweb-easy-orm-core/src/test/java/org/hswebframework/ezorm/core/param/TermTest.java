package org.hswebframework.ezorm.core.param;

import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.*;

public class TermTest {

    @Test
    public void testTermTypeAndOptionsByColumn() {
        Term term = new Term();
        term.setColumn("name$in$any");

        Assert.assertEquals(term.getColumn(), "name");
        Assert.assertEquals(term.getTermType(), "in");
        Assert.assertTrue(term.getOptions().contains("any"));

        term.addTerm(term);

        Assert.assertNotEquals(term.getTerms().get(0), term);
    }

    @Test
    public void testTermTypeOptionsByTermType() {
        Term term = new Term();
        term.setColumn("name");
        term.setTermType("in$any$opt2");

        Assert.assertEquals(term.getColumn(), "name");
        Assert.assertEquals(term.getTermType(), "in");
        Assert.assertTrue(term.getOptions().contains("any"));
        Assert.assertTrue(term.getOptions().contains("opt2"));

    }

    @Test
    public void testAnd() {
        Term term = new Term();
        term.and("test", "1");
        Assert.assertEquals(term.getTerms().get(0).getColumn(), "test");
        Assert.assertEquals(term.getTerms().get(0).getValue(), "1");
        Assert.assertEquals(term.getTerms().get(0).getType(), Term.Type.and);

    }

    @Test
    public void testOr() {
        Term term = new Term();
        term.or("test", "1");
        Assert.assertEquals(term.getTerms().get(0).getColumn(), "test");
        Assert.assertEquals(term.getTerms().get(0).getValue(), "1");
        Assert.assertEquals(term.getTerms().get(0).getType(), Term.Type.or);

    }

    @Test
    public void testNest() {
        Term term = new Term().nest().and("test", "1").clone();
        Assert.assertEquals(term.getTerms().get(0).getColumn(), "test");
        Assert.assertEquals(term.getTerms().get(0).getValue(), "1");
        Assert.assertEquals(term.getTerms().get(0).getType(), Term.Type.and);
    }

    @Test
    public void testOrNest() {
        Term term = new Term().orNest().or("test", "1").clone();

        Assert.assertEquals(term.getType(), Term.Type.or);
        Assert.assertEquals(term.getTerms().get(0).getColumn(), "test");
        Assert.assertEquals(term.getTerms().get(0).getValue(), "1");
        Assert.assertEquals(term.getTerms().get(0).getType(), Term.Type.or);
    }


}