package org.hswebframework.ezorm.core.dsl;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.param.Param;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.UpdateParam;
import org.junit.Assert;
import org.junit.Test;

import java.util.Map;

public class UpdateDeleteTest {

    @Test
    public void testUpdateMapDsl() {
        Update<Map<String, Object>, UpdateParam<Map<String, Object>>> update = Update.of();

        update.set("name", "JetLinks")
              .set(Map.of("state", 1))
              .includes("name")
              .excludes("ignore")
              .where("id", "eq", "test")
              .or("name", "like", "Jet")
              .and("ignored", "eq", null);

        Assert.assertEquals("JetLinks", update.getParam().getData().get("name"));
        Assert.assertEquals(1, update.getParam().getData().get("state"));
        Assert.assertTrue(update.getParam().getIncludes().contains("name"));
        Assert.assertTrue(update.getParam().getExcludes().contains("ignore"));
        Assert.assertEquals(2, update.getParam().getTerms().size());
        Assert.assertEquals("test", update.execute(param -> param.getTerms().get(0).getValue()));
    }

    @Test
    public void testUpdateBeanDslAndNest() {
        Entity entity = new Entity();
        Update<Entity, UpdateParam<Entity>> update = Update.of(entity);

        update.set(Entity::getName, "JetLinks")
              .includes(Entity::getName)
              .excludes(Entity::getIgnored)
              .nest()
              .and("id", "eq", "1")
              .orNest("name", "like")
              .and("state", "eq", 1)
              .end()
              .end();

        Assert.assertEquals("JetLinks", entity.getName());
        Assert.assertTrue(update.getParam().getIncludes().contains("name"));
        Assert.assertTrue(update.getParam().getExcludes().contains("ignored"));
        Assert.assertEquals(1, update.getParam().getTerms().size());
        Term root = update.getParam().getTerms().get(0);
        Assert.assertEquals(2, root.getTerms().size());
        Assert.assertEquals(Term.Type.or, root.getTerms().get(1).getType());
    }

    @Test
    public void testDeleteDsl() {
        Delete<Param> delete = Delete.of();

        delete.where("id", "eq", "test")
              .or("name", "like", "Jet")
              .and("ignored", "eq", null)
              .nest()
              .and("state", "eq", 1)
              .end();

        Assert.assertEquals(3, delete.getParam().getTerms().size());
        Assert.assertEquals("test", delete.execute(param -> param.getTerms().get(0).getValue()));

        Param custom = new Param();
        Delete<Param> supplied = Delete.of(() -> custom);
        Assert.assertSame(custom, supplied.getParam());
        Assert.assertSame(supplied, supplied.setParam(new Param()));
    }

    @Test(expected = NullPointerException.class)
    public void testUpdateRejectsNullData() {
        Update.of((Map<String, Object>) null);
    }

    @Test(expected = NullPointerException.class)
    public void testUpdateRejectsNullParamData() {
        Update.of(new UpdateParam<>(null));
    }

    @Getter
    @Setter
    public static class Entity {
        private String id;
        private String name;
        private String ignored;
    }
}
