package org.hswebframework.ezorm.rdb.utils;

import org.junit.Assert;
import org.junit.Test;

import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Method;
import java.util.Set;

public class AnnotationUtilsTest {

    @Test
    public void testGetFiledByDescriptorAcrossHierarchyAndCapitalization() throws Exception {
        PropertyDescriptor descriptor = new PropertyDescriptor("name", Child.class, "getName", "setName");
        Assert.assertTrue(AnnotationUtils.getFiledByDescriptor(Child.class, descriptor).isPresent());
        Assert.assertEquals("name", AnnotationUtils.getFiledByDescriptor(Child.class, descriptor).get().getName());

        PropertyDescriptor capitalized = new PropertyDescriptor("Name", Child.class, "getName", "setName");
        Assert.assertTrue(AnnotationUtils.getFiledByDescriptor(Child.class, capitalized).isPresent());

        PropertyDescriptor missing = new PropertyDescriptor("missing", Child.class, "getName", "setName");
        Assert.assertFalse(AnnotationUtils.getFiledByDescriptor(Child.class, missing).isPresent());
    }

    @Test
    public void testGetAnnotationFromMethodFieldAndSuperclass() throws Exception {
        PropertyDescriptor descriptor = new PropertyDescriptor("name", Child.class, "getName", "setName");
        Assert.assertEquals(Marker.class, AnnotationUtils.getAnnotation(Child.class, descriptor, Marker.class).annotationType());
        Assert.assertEquals(Marker.class, AnnotationUtils.getAnnotation(Child.class, descriptor, Marker.class).annotationType());

        Method method = Child.class.getMethod("getName");
        Assert.assertEquals(Marker.class, AnnotationUtils.getAnnotation(method, Marker.class).annotationType());
        Assert.assertNull(AnnotationUtils.getAnnotation(NoMarker.class, Marker.class));
    }

    @Test
    public void testGetAnnotationsDeduplicatesByType() throws IntrospectionException {
        PropertyDescriptor descriptor = new PropertyDescriptor("name", Child.class, "getName", "setName");
        Set<java.lang.annotation.Annotation> annotations = AnnotationUtils.getAnnotations(Child.class, descriptor);
        Assert.assertEquals(1, annotations.stream().filter(a -> a.annotationType() == Marker.class).count());
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.FIELD, ElementType.METHOD, ElementType.TYPE})
    public @interface Marker {
    }

    public static class Base {
        @Marker
        private String name;

        @Marker
        public String getName() {
            return name;
        }
    }

    public static class Child extends Base {
        @Override
        @Marker
        public String getName() {
            return super.getName();
        }

        public void setName(String name) {
            try {
                java.lang.reflect.Field field = Base.class.getDeclaredField("name");
                field.setAccessible(true);
                field.set(this, name);
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
        }
    }

    public static class NoMarker {
        public String getOther() {
            return "x";
        }
    }
}
