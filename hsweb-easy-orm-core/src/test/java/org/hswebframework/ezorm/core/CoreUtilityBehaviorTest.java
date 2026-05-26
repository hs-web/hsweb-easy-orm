package org.hswebframework.ezorm.core;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.utils.StringUtils;
import org.junit.Assert;
import org.junit.Test;

import java.io.Serializable;
import java.util.*;

public class CoreUtilityBehaviorTest {

    @Test
    public void testStringUtilsForSqlNameAndDynamicParams() {
        Assert.assertTrue(StringUtils.isNullOrEmpty(null));
        Assert.assertTrue(StringUtils.isNullOrEmpty(""));
        Assert.assertTrue(StringUtils.isNullOrEmpty(Collections.emptyList()));
        Assert.assertTrue(StringUtils.isNullOrEmpty(Collections.emptyMap()));
        Assert.assertFalse(StringUtils.isNullOrEmpty("device"));
        Assert.assertFalse(StringUtils.isNullOrEmpty(Collections.singletonList("id")));

        Assert.assertEquals("a,b,c", StringUtils.join(",", Arrays.asList("a", "b", "c")));
        Assert.assertEquals("device.name", StringUtils.concat("device", ".", "name"));
        Assert.assertEquals("device", StringUtils.toLowerCaseFirstOne("Device"));
        Assert.assertEquals("device", StringUtils.toLowerCaseFirstOne("device"));
        Assert.assertArrayEquals(new String[]{"a", "b", "c"}, StringUtils.split("a.b.c", '.'));
        Assert.assertArrayEquals(new String[]{"a", "b"}, StringUtils.split("a.b.", '.'));
        Assert.assertEquals("table", StringUtils.getPlainName("`table`"));
        Assert.assertEquals("table", StringUtils.getPlainName("\"table\""));
        Assert.assertEquals("table", StringUtils.getPlainName("[table]"));
        Assert.assertArrayEquals(new String[]{"id", "name"}, StringUtils.getPlainName(new String[]{"`id`", "[name]"}));
    }

    @Test
    public void testSimplePropertyWrapperConversions() {
        Assert.assertEquals(12, new SimplePropertyWrapper(12L).toInt());
        Assert.assertEquals(12, new SimplePropertyWrapper("12").toInt());
        Assert.assertEquals(12.5D, new SimplePropertyWrapper("12.5").toDouble(), 0.0001D);
        Assert.assertTrue(new SimplePropertyWrapper(true).isTrue());
        Assert.assertTrue(new SimplePropertyWrapper(1).isTrue());
        Assert.assertTrue(new SimplePropertyWrapper("yes").isTrue());
        Assert.assertFalse(new SimplePropertyWrapper("no").isTrue());
        Assert.assertTrue(new SimplePropertyWrapper(Collections.emptyList()).isNullOrEmpty());
        Assert.assertFalse(new SimplePropertyWrapper(null).valueTypeOf(String.class));
        Assert.assertTrue(new SimplePropertyWrapper("value").valueTypeOf(String.class));

        Date now = new Date();
        Assert.assertSame(now, new SimplePropertyWrapper(now).toDate());
        Assert.assertEquals(now, new SimplePropertyWrapper(now).toDate("yyyy-MM-dd"));

        Device device = new SimplePropertyWrapper("{\"id\":\"dev1\",\"count\":3}").toBean(Device.class);
        Assert.assertEquals("dev1", device.getId());
        Assert.assertEquals(3, device.getCount());
        Assert.assertEquals("dev1", new SimplePropertyWrapper("{\"id\":\"dev1\"}").toMap().get("id"));
        Assert.assertEquals("dev2", new SimplePropertyWrapper("[{\"id\":\"dev2\"}]").toBeanList(Device.class).get(0).getId());
        Assert.assertEquals("dev3", ((Map<?, ?>) new SimplePropertyWrapper("[{\"id\":\"dev3\"}]").toList().get(0)).get("id"));

        List<Device> devices = Collections.singletonList(device);
        Assert.assertSame(devices, new SimplePropertyWrapper(devices).toBeanList(Device.class));
        Assert.assertSame(device, new SimplePropertyWrapper(device).toBean(Device.class));
    }

    @Test
    public void testObjectPropertyOperatorBeanAndExtensionBehavior() {
        ApacheCommonPropertyOperator operator = ApacheCommonPropertyOperator.INSTANCE;
        Device source = new Device();
        source.setId("dev1");
        source.setCount(7);

        Assert.assertEquals("dev1", operator.getProperty(source, "id").orElse(null));
        Assert.assertFalse(operator.getProperty(source, "missing").isPresent());
        Assert.assertEquals(String.class, operator.getPropertyType(source, "id").orElse(null));
        Assert.assertFalse(operator.getPropertyType(source, "missing").isPresent());

        operator.setProperty(source, "count", "8");
        Assert.assertEquals(8, source.getCount());

        Device copied = operator.convert(source, Device.class);
        Assert.assertEquals(source.getId(), copied.getId());
        Assert.assertEquals(source.getCount(), copied.getCount());

        Map<String, Object> map = operator.convert(source, LinkedHashMap::new);
        Assert.assertEquals("dev1", map.get("id"));
        Assert.assertEquals(8, map.get("count"));

        ExtensionDevice ext = new ExtensionDevice();
        operator.setProperty(ext, "dynamic", null);
        Assert.assertTrue(ext.extensions().containsKey("dynamic"));
        ext.withExtension(methodColumn("runtime", "online"));
        ext.withExtension(staticColumn("id"), new Device());
        ext.setExtension("intValue", 1);
        ext.setExtension("longValue", 2L);
        ext.setExtension("doubleValue", 3D);
        ext.setExtension("floatValue", 4F);
        ext.setExtension("booleanValue", true);
        ext.setExtension("byteValue", (byte) 5);
        ext.setExtension("charValue", 'c');
        ext.setExtension("shortValue", (short) 6);
        Assert.assertEquals("online", ext.getExtension("runtime"));
        Assert.assertTrue(ext.getExtension("id") instanceof Device);

        ExtensibleDevice oldExt = new ExtensibleDevice();
        oldExt.setExtension(methodColumn("legacy", "ok"));
        oldExt.setExtension(staticColumn("id"), new Device());
        oldExt.setExtension("intValue", 1);
        oldExt.setExtension("longValue", 2L);
        oldExt.setExtension("doubleValue", 3D);
        oldExt.setExtension("floatValue", 4F);
        oldExt.setExtension("booleanValue", true);
        oldExt.setExtension("byteValue", (byte) 5);
        oldExt.setExtension("charValue", 'c');
        oldExt.setExtension("shortValue", (short) 6);
        Assert.assertEquals("ok", oldExt.getExtension("legacy"));
        Assert.assertNull(new ExtensibleDevice(false).getExtension("missing"));
    }

    @Test
    public void testObjectPropertyCompareAndAutoCreateNestedBean() {
        ApacheCommonPropertyOperator operator = ApacheCommonPropertyOperator.INSTANCE;
        Device device = new Device();

        Assert.assertEquals(0, operator.compare("a", "a"));
        Assert.assertTrue(operator.compare("a", "b") < 0);
        Assert.assertTrue(operator.compare(1, 2L) < 0);
        Assert.assertEquals(-1, operator.compare(new Object(), new Object()));

        Object nested = operator.getPropertyOrNew(device, "nested");
        Assert.assertTrue(nested instanceof Nested);
        Assert.assertSame(nested, device.getNested());
        Assert.assertNull(operator.getPropertyOrNew(device, "missing"));
    }

    private static <T> MethodReferenceColumn<T> methodColumn(String column, T value) {
        return new TestMethodReferenceColumn<>(column, value);
    }

    private static <T> StaticMethodReferenceColumn<T> staticColumn(String column) {
        return new TestStaticMethodReferenceColumn<>(column);
    }

    static class TestStaticMethodReferenceColumn<T> implements StaticMethodReferenceColumn<T> {
        private final String column;

        TestStaticMethodReferenceColumn(String column) {
            this.column = column;
        }

        @Override
        public String getColumn() {
            return column;
        }

        @Override
        public Object apply(T value) {
            return null;
        }
    }

    static class TestMethodReferenceColumn<T> implements MethodReferenceColumn<T> {
        private final String column;
        private final T value;

        TestMethodReferenceColumn(String column, T value) {
            this.column = column;
            this.value = value;
        }

        @Override
        public String getColumn() {
            return column;
        }

        @Override
        public T get() {
            return value;
        }
    }

    @Getter
    @Setter
    public static class Device implements Serializable {
        private String id;
        private int count;
        private Nested nested;
    }

    public static class Nested implements Serializable {
    }

    public static class ExtensionDevice implements Extendable {
        private final Map<String, Object> extensions = new LinkedHashMap<>();

        @Override
        public Map<String, Object> extensions() {
            return extensions;
        }

        @Override
        public void setExtension(String property, Object value) {
            extensions.put(property, value);
        }
    }

    @Deprecated
    public static class ExtensibleDevice implements Extensible {
        private final Map<String, Object> extensions;

        public ExtensibleDevice() {
            this(true);
        }

        public ExtensibleDevice(boolean createExtensions) {
            this.extensions = createExtensions ? new LinkedHashMap<>() : null;
        }

        @Override
        public Map<String, Object> extensions() {
            return extensions;
        }

        @Override
        public void setExtension(String property, Object value) {
            extensions.put(property, value);
        }
    }
}
