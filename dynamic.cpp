
#include <map>
#include <vector>
#include <string>
#include <functional>

class Object {
private:

	enum TypeTag {
		T_Int,
		T_Double,
		T_String,
		T_Attrs,
		T_List,
		T_Func
	};

	TypeTag type;

	using Int    = int;
	using Double = double;
	using String = std::string;
	using Attrs  = std::map<String, Object*>;
	using List   = std::vector<Object*>;
	using Func   = std::function<Object*(List)>;

	union {
		Int    vInt;
		Double vDouble;
		String vString;
		Attrs  vAttrs;
		List   vList;
		Func   vFunc;
	};

	Int& getInt() {
		if (type != T_Int)
			throw Object("Unexpected Int");
		return vInt;
	}

#define GETTER(ty) \
		ty& get##ty() { if (type != T_##ty) throw Object("Unexpected " #ty); return v##ty; }
	GETTER(Double)
	GETTER(String)
	GETTER(Attrs)
	GETTER(List)
	GETTER(Func)
#undef GETTER

public:
	Object(const char* str) : type(T_String) { new (&vString) String(str); }
	Object(const Object& obj) : type(obj.type) {
		switch (type) {
			case T_Int: new (&vInt) Int(obj.vInt); break;
			// TODO other cases
		}
	}
	~Object() { /* TODO */ }
};

using var = Object;


