def generate_association(rel):
    name = f"{rel['principal']}_{rel['dependent']}"

    return f"""
    <Association Name="{name}">
        <End Type="Mock.{rel['principal']}"
             Role="{rel['principal']}"
             Multiplicity="{rel['multiplicity_principal']}" />
        <End Type="Mock.{rel['dependent']}"
             Role="{rel['dependent']}"
             Multiplicity="{rel['multiplicity_dependent']}" />
        <ReferentialConstraint>
            <Principal Role="{rel['principal']}">
                <PropertyRef Name="{rel['principal_key']}" />
            </Principal>
            <Dependent Role="{rel['dependent']}">
                <PropertyRef Name="{rel['dependent_key']}" />
            </Dependent>
        </ReferentialConstraint>
    </Association>
    """