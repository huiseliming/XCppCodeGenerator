
#include <filesystem>
#include <fstream>
#include <unordered_map>
#include <unordered_set>

#include "clang/AST/RecursiveASTVisitor.h"
#include <clang/ASTMatchers/ASTMatchFinder.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendActions.h>
#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Tooling.h>
#include <llvm/Support/CommandLine.h>

std::vector<std::pair<std::string, std::string>>
ParseAttrs(const std::string& expr);

using namespace llvm;

llvm::cl::OptionCategory option_category("CppKitHeaderTool");

static cl::opt<std::string> src_dir("src_dir", cl::desc("source directory"), cl::Required, cl::cat(option_category));
static cl::opt<std::string> gen_dir("gen_dir", cl::desc("code generate directory"), cl::Required, cl::cat(option_category));
static cl::list<std::string> target_list("target", cl::desc("reflection generated target header list"),
                                         cl::cat(option_category));

std::string
ConvertDeclToUnique(const std::string& decl_name) {
    // llvm::outs() << "decl_name : " << decl_name << "\n";
    std::string unique_name;
    for (size_t i = 0; i < decl_name.size(); i++) {
        switch (decl_name[i]) {
        case '_':
            unique_name.append("___");
            break;
        case ',':
            unique_name.append("_v_");
            break;
        case '<':
            unique_name.append("_d_");
            break;
        case '>':
            unique_name.append("_b_");
        case '(':
            unique_name.append("_9_");
            break;
        case ')':
            unique_name.append("_0_");
            break;
        case ':':
            if (i + 1 < decl_name.size() && decl_name[i + 1] == ':') {
                unique_name.append("_n_");
                i++;
            } else {
                return "";
            }
            break;
        case ' ':
            if (i > 0 && (std::isalnum(decl_name[i - 1]) || decl_name[i - 1] == '_') && i + 1 < decl_name.size()) {
                // skip the space
                while (decl_name[i + 1] == ' ') {
                    i++;
                }
                if (std::isalnum(decl_name[i + 1]) || decl_name[i + 1] == '_') {
                    unique_name.append("_s_");
                }
            }
            break;
        case '*':
            unique_name.append("_p_");
            break;
        case '&':
            unique_name.append("_r_");
            break;
        default:
            unique_name.push_back(decl_name[i]);
            break;
        }
    }
    // llvm::outs() << "unique_name : " << unique_name << "\n";
    return unique_name;
}

std::string
ConvertUniqueToDecl(const std::string& unique_name) {
    std::string decl_name;
    for (size_t i = 0; i < unique_name.size(); i++) {
        if (unique_name[i] == '_') {
            if (i + 2 < unique_name.size()) {
                i++;
                switch (unique_name[i]) {
                case '_':
                    decl_name.push_back('_');
                    break;
                case 'v':
                    decl_name.push_back(',');
                    break;
                case 'd':
                    decl_name.push_back('<');
                    break;
                case 'b':
                    decl_name.push_back('>');
                    break;
                case '9':
                    decl_name.append("_(_");
                    break;
                case '0':
                    decl_name.append("_)_");
                    break;
                case 'n':
                    decl_name.append("::");
                    break;
                case 's':
                    decl_name.push_back(' ');
                    break;
                case 'p':
                    decl_name.push_back('*');
                    break;
                case 'r':
                    decl_name.push_back('&');
                    break;
                default:
                    return "";
                }
                i++;
            }
        } else {
            decl_name.push_back(unique_name[i]);
        }
    }
    return decl_name;
}

namespace fs = std::filesystem;
class HeaderToolMatchCallback : public clang::ast_matchers::MatchFinder::MatchCallback {
  public:
    void
    run(const clang::ast_matchers::MatchFinder::MatchResult& match_result) {
        clang::ASTContext* ast_context = match_result.Context;
        clang::SourceManager* source_manager = match_result.SourceManager;
        ASTCtx = ast_context;
        if (const clang::NamedDecl* decl = match_result.Nodes.getNodeAs<clang::NamedDecl>("Decl")) {
            const clang::NamedDecl* attrs_cache_decl = decl;
            auto filename = source_manager->getFilename(source_manager->getFileLoc(decl->getLocation()));
            auto it = DeclsMap.find(filename.str());
            if (it != DeclsMap.end()) {
                if (const clang::CXXRecordDecl* forward_cxxrecord_decl = dyn_cast<clang::CXXRecordDecl>(decl)) {
                    if (const clang::CXXRecordDecl* cxxrecord_decl = dyn_cast<clang::CXXRecordDecl>(
                            forward_cxxrecord_decl->getNextDeclInContext())) {
                        decl = cxxrecord_decl;
                        llvm::outs() << "CXXRecordDecl : " << cxxrecord_decl->getName() << "\n";
                    } else if (const clang::EnumDecl* enum_decl = dyn_cast<clang::EnumDecl>(
                                   forward_cxxrecord_decl->getNextDeclInContext())) {
                        decl = enum_decl;
                    }
                } else if (const clang::FieldDecl* field_decl = dyn_cast<clang::FieldDecl>(decl)) {
                    decl = field_decl;
                    llvm::outs() << "FieldDecl : " << field_decl->getName() << "\n";
                } else if (const clang::FunctionDecl* function_decl = dyn_cast<clang::FunctionDecl>(decl)) {
                    decl = function_decl;
                    llvm::outs() << "FunctionDecl : " << function_decl->getName() << "\n";
                } else if (const clang::VarDecl* var_decl = dyn_cast<clang::VarDecl>(decl)) {
                    decl = var_decl;
                }
                if (decl) {
                    it->second.push_back(decl);
                    for (auto attr : attrs_cache_decl->getAttrs()) {
                        if (attr->getKind() == clang::attr::Annotate) {
                            std::string raw_string_buffer;
                            llvm::raw_string_ostream raw_string_output_stream(raw_string_buffer);
                            attr->printPretty(raw_string_output_stream, clang::PrintingPolicy(clang::LangOptions()));
                            if (raw_string_buffer.starts_with(" [[clang::annotate(\"@") &&
                                raw_string_buffer.ends_with("\")]]")) {
                                AttrsMap.insert(std::pair(
                                    decl,
                                    raw_string_buffer.substr(21, raw_string_buffer.size() - sizeof(" [[clang::annotate(\"@") +
                                                                     1 - sizeof("\")]]") + 1)));
                            }
                        }
                    }
                }
            }
        }
    }

    std::string
    ToAngelScriptTypeDecl(clang::QualType type) {
        if (type.isNull())
            return "?";
        std::string astype_decl = type.getAsString();
        clang::QualType final_type;
        auto pointee_type = type;
        while (!pointee_type.isNull()) {
            final_type = pointee_type;
            pointee_type = final_type->getPointeeType();
        }
        if (final_type->isBooleanType()) {
            size_t find_std_pos = astype_decl.find("_Bool");
            if (find_std_pos != std::string::npos) {
                astype_decl.replace(find_std_pos, sizeof("_Bool") - 1, "bool");
            }
        } else if (auto cxxrecord_decl = final_type->getAsCXXRecordDecl()) {
            if (cxxrecord_decl->getEnclosingNamespaceContext()->isStdNamespace()) {
                size_t find_std_pos = astype_decl.find("std::");
                while (find_std_pos != std::string::npos) {
                    astype_decl.replace(find_std_pos, sizeof("std::") - 1, "");
                    find_std_pos = astype_decl.find("std::", find_std_pos);
                }
            }
        } else {
        }
        std::for_each(astype_decl.begin(), astype_decl.end(), [](char& c) {
            if (c == '*')
                c = '&';
        });
        return astype_decl;
    }

    inline std::string
    ToNamespaceString(const clang::NamedDecl* decl) {
        auto decl_ctx = decl->getDeclContext();
        if (isa<clang::NamespaceDecl>(decl_ctx)) {
            const clang::NamespaceDecl* ns_decl = cast<clang::NamespaceDecl>(decl_ctx);
            return std::format("\"{}\"", ns_decl->getQualifiedNameAsString());
        } else if (isa<clang::CXXRecordDecl>(decl_ctx)) {
            const clang::CXXRecordDecl* cxxrecord_decl = cast<clang::CXXRecordDecl>(decl_ctx);
            return std::format("\"{}\"", cxxrecord_decl->getQualifiedNameAsString());
        }
        return "static_cast<const char*>(nullptr)";
    }

    std::string
    GetDeclUniqueName(const clang::NamedDecl* decl) {
        if (const clang::FunctionDecl* function_decl = dyn_cast<clang::FunctionDecl>(decl)) {
            std::string unique_name = std::format(
                "{} {}(", function_decl->getReturnType().getAsString(), function_decl->getQualifiedNameAsString());
            if (function_decl->getNumParams() > 0) {
                for (size_t i = 0; i < function_decl->getNumParams() - 1; i++) {
                    unique_name += function_decl->getParamDecl(i)->getType().getAsString() + ", ";
                }
                unique_name += function_decl->getParamDecl(function_decl->getNumParams() - 1)->getType().getAsString();
            }
            unique_name += ")";
            return ConvertDeclToUnique(unique_name);
        } else {
            return ConvertDeclToUnique(decl->getQualifiedNameAsString());
        }
    }

    virtual void
    onStartOfTranslationUnit() {
        llvm::outs() << "onStartOfTranslationUnit\n";
    }

    virtual void
    onEndOfTranslationUnit() {
        llvm::outs() << "onEndOfTranslationUnit\n";
        for (auto& [src_file_path, decls] : DeclsMap) {
            std::error_code error_code;
            auto relative_path = fs::relative(src_file_path, fs::path(src_dir.c_str()), error_code);
            auto gen_header_file_path = gen_dir.getValue() / relative_path.replace_extension(".gen.h");
            auto gen_source_file_path = gen_dir.getValue() / relative_path.replace_extension(".cpp");
            std::string gen_header_code = "// this is a generated header file \n";
            gen_header_code += "#pragma once\n";
            gen_header_code += "#include \"" + src_file_path.string() + "\"\n";
            std::string gen_source_code = "// this is a generated source file \n";
            if (!decls.empty()) {
                gen_source_code += "#include \"Core/DeclManager.h\"\n";
                gen_source_code += "#include \"" + gen_header_file_path.string() + "\"\n";

                std::string gen_namespace_path = "GNRT";
                for (auto& part : relative_path) {
                    std::string ns_name = part.string();
                    std::for_each(ns_name.begin(), ns_name.end(), [](char& c) {
                        if (!std::isalnum(c))
                            c = '_';
                    });
                    gen_namespace_path += "::" + ns_name;
                }

                gen_source_code += "namespace " + gen_namespace_path + " { class _ { }; }\n";
                gen_source_code += "using _ = ::" + gen_namespace_path + "::_;\n";
                gen_source_code += "template <>\n";
                gen_source_code += "struct TCodeContainer<_> {\n";
                gen_source_code += "  TCodeContainer() {\n";
                gen_source_code += "    CDeclManager* dm = &GetDeclManager();\n";
                gen_source_code += "    RDecl* d = nullptr;\n"; // ctx decl
                for (auto& decl : decls) {
                    gen_source_code += std::format("    \n");
                    std::string decl_name = decl->getNameAsString();
                    std::string qual_decl_name = decl->getQualifiedNameAsString();
                    std::string unique_decl_name;
                    std::string asdecl;
                    std::vector<std::pair<std::string, std::string>> name_remap_attrs;
                    if (isa<clang::FunctionDecl>(decl)) {
                        unique_decl_name = GetDeclUniqueName(cast<clang::FunctionDecl>(decl));
                    } else {
                        unique_decl_name = ConvertDeclToUnique(qual_decl_name);
                    }
                    std::string owner_name;
                    auto decl_ctx = decl->getDeclContext();
                    if (isa<clang::NamespaceDecl>(decl_ctx)) {
                        owner_name = std::format("\"{}\"", cast<clang::NamespaceDecl>(decl_ctx)->getQualifiedNameAsString());
                    } else if (isa<clang::CXXRecordDecl>(decl_ctx)) {
                        owner_name = std::format(
                            "&GNRT_{}", ConvertDeclToUnique(cast<clang::CXXRecordDecl>(decl_ctx)->getQualifiedNameAsString()));
                    } else {
                        owner_name = "static_cast<const char*>(nullptr)";
                    }
                    if (const clang::CXXRecordDecl* cxxrecord_decl = dyn_cast<clang::CXXRecordDecl>(decl)) {
                        auto cxxbase = cxxrecord_decl->bases_begin();
                        const clang::CXXRecordDecl* first_base_decl = nullptr;
                        std::vector<const clang::CXXRecordDecl*> other_base_decls;
                        if (cxxbase != cxxrecord_decl->bases_end()) {
                            first_base_decl = cxxbase->getType()->getAsCXXRecordDecl();
                            for (cxxbase++; cxxbase != cxxrecord_decl->bases_end(); cxxbase++) {
                                if (auto base_decl = cxxbase->getType()->getAsCXXRecordDecl()) {
                                    other_base_decls.push_back(base_decl);
                                }
                            }
                        }
                        std::string base_name = first_base_decl ? first_base_decl->getQualifiedNameAsString() : "void";
                        gen_source_code += std::format(R"(    static TClass<{}, {}> GNRT_{}("{}", {}, dm);
)",
                                                       qual_decl_name, base_name, unique_decl_name, decl_name, owner_name);
                        asdecl = std::format("class {}", decl_name);
                    } else if (const clang::EnumDecl* enum_decl = dyn_cast<clang::EnumDecl>(decl)) {
                        gen_source_code += std::format(
                            R"(    static TEnum<{}> GNRT_{}("{}", {}, dm);
)",
                            qual_decl_name, unique_decl_name, decl_name, owner_name);
                        gen_source_code += std::format("    GNRT_{}.EnumValues = {{", unique_decl_name);

                        for (auto enumerator_it = enum_decl->enumerator_begin(); enumerator_it != enum_decl->enumerator_end();
                             enumerator_it++) {
                            gen_source_code += "{" +
                                               std::format(R"("{}", {})", enumerator_it->getNameAsString(),
                                                           enumerator_it->getQualifiedNameAsString()) +
                                               "},";
                            if (enumerator_it->hasAttrs()) {
                                for (auto attr : enumerator_it->getAttrs()) {
                                    if (attr->getKind() == clang::attr::Annotate) {
                                        std::string raw_string_buffer;
                                        llvm::raw_string_ostream raw_string_output_stream(raw_string_buffer);
                                        attr->printPretty(
                                            raw_string_output_stream, clang::PrintingPolicy(clang::LangOptions()));
                                        if (raw_string_buffer.starts_with(" [[clang::annotate(\"@") &&
                                            raw_string_buffer.ends_with("\")]]")) {
                                            std::string custom_name = raw_string_buffer.substr(
                                                sizeof(" [[clang::annotate(\"@") - 1, raw_string_buffer.size() -
                                                                                          sizeof(" [[clang::annotate(\"@") + 1 -
                                                                                          sizeof("\")]]") + 1);
                                            if (!custom_name.empty()) {
                                                if (custom_name.starts_with("\"") && custom_name.ends_with("\"")) {
                                                    name_remap_attrs.push_back(
                                                        {std::format(R"(fmt::format(">{{:d}}", fmt::underlying({})))",
                                                                     enumerator_it->getQualifiedNameAsString()),
                                                         custom_name});
                                                } else {
                                                    name_remap_attrs.push_back(
                                                        {std::format(R"(fmt::format(">{{:d}}", fmt::underlying({})))",
                                                                     enumerator_it->getQualifiedNameAsString()),
                                                         std::format(R"(CK_STRINGIFY({}))", custom_name)});
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        gen_source_code += "};\n";
                        asdecl = std::format("enum {}", decl_name);
                        // decl_var_name = std::format("CK_ENUM_NAME({});\n", enum_decl->getNameAsString());
                    } else if (const clang::FieldDecl* field_decl = dyn_cast<clang::FieldDecl>(decl)) {
                        if (cxxrecord_decl = dyn_cast<clang::CXXRecordDecl const>(field_decl->getParent())) {
                            std::string class_qual_name = cxxrecord_decl->getQualifiedNameAsString();
                            std::string field_type = field_decl->getType().getAsString();
                            gen_source_code += std::format(
                                R"(    static TMemberVariable<{}> GNRT_{}("{}", asOFFSET({}, {}), {}, dm);
)",
                                field_type, unique_decl_name, decl_name, class_qual_name, decl_name, owner_name);

                            auto astype_decl = ToAngelScriptTypeDecl(field_decl->getType());
                            asdecl = std::format("{} {}", astype_decl, decl_name);
                        }
                    } else if (const clang::FunctionDecl* function_decl = dyn_cast<clang::FunctionDecl>(decl)) {
                        std::string return_type, params_type;
                        std::string asdecl_return_type;
                        std::string asdecl_params_type;
                        std::string init_member_function_macro;
                        return_type = function_decl->getReturnType().getAsString();
                        asdecl_return_type = ToAngelScriptTypeDecl(function_decl->getReturnType());
                        if (function_decl->getNumParams() > 0) {
                            for (size_t i = 0; i < function_decl->getNumParams() - 1; i++) {
                                params_type += function_decl->getParamDecl(i)->getType().getAsString() + ", ";
                                asdecl_params_type += ToAngelScriptTypeDecl(function_decl->getParamDecl(i)->getType()) + ", ";
                            }
                            params_type +=
                                function_decl->getParamDecl(function_decl->getNumParams() - 1)->getType().getAsString();
                            asdecl_params_type += ToAngelScriptTypeDecl(
                                function_decl->getParamDecl(function_decl->getNumParams() - 1)->getType());
                        } else {
                            params_type += "";
                        }
                        std::string function_type = std::format("{} {}({})", return_type, decl_name, params_type);
                        if (cxxrecord_decl = dyn_cast<clang::CXXRecordDecl const>(function_decl->getParent())) {
                            if (const clang::CXXMethodDecl* method_decl = dyn_cast<clang::CXXMethodDecl>(decl)) {
                                std::string class_qual_name = cxxrecord_decl->getQualifiedNameAsString();
                                if (method_decl->isStatic()) {
                                    gen_source_code += std::format(
                                        R"(    static TGlobalFunction<BOOST_TYPEOF(static_cast<{} (*)({})>(&{}::{}))> GNRT_{}("{}", asFUNCTIONPR(&{}::{}, ({}), {}), {}, dm);
)",
                                        return_type, params_type, class_qual_name, decl_name, unique_decl_name, function_type,
                                        class_qual_name, decl_name, params_type, return_type, owner_name);
                                } else {
                                    gen_source_code += std::format(
                                        R"(    static TMemberFunction<BOOST_TYPEOF(static_cast<{} ({}::*)({})>(&{}::{}))> GNRT_{}("{}", asMETHODPR({}, {}, ({}), {}), {}, dm);
)",
                                        return_type, class_qual_name, params_type, class_qual_name, decl_name, unique_decl_name,
                                        function_type, class_qual_name, decl_name, params_type, return_type, owner_name);
                                }
                            }
                        } else {
                            gen_source_code += std::format(
                                R"(    static TGlobalFunction<BOOST_TYPEOF(static_cast<{} (*)({})>(&{}))> GNRT_{}("{}", asFUNCTIONPR(&{}, ({}), {}), {}, dm);
)",
                                return_type, params_type, qual_decl_name, unique_decl_name, function_type, qual_decl_name,
                                params_type, return_type, owner_name);
                        }
                        gen_source_code += init_member_function_macro;
                        asdecl = std::format("{} {}({})", asdecl_return_type, decl_name, asdecl_params_type);
                    } else if (const clang::VarDecl* var_decl = dyn_cast<clang::VarDecl>(decl)) {
                        std::string var_type = var_decl->getType().getAsString();
                        if (var_decl->isCXXClassMember()) {
                            if (auto decl_context = var_decl->getDeclContext()) {
                                if (const clang::CXXRecordDecl* cxxrecord_decl = dyn_cast<clang::CXXRecordDecl>(decl_context)) {
                                    std::string class_qual_name = cxxrecord_decl->getQualifiedNameAsString();
                                    gen_source_code += std::format(
                                        R"(    static TGlobalVariable<{}> GNRT_{}("{}", &{}::{}, {}, dm);
)",
                                        var_type, unique_decl_name, decl_name, class_qual_name, decl_name, owner_name);
                                }
                            }
                        } else {
                            gen_source_code += std::format(R"(    static TGlobalVariable<{}> GNRT_{}("{}", &{}, {}, dm);
)",
                                                           var_type, unique_decl_name, decl_name, qual_decl_name, owner_name);
                        }
                        auto astype_decl = ToAngelScriptTypeDecl(var_decl->getType());
                        asdecl = std::format("{} {}", astype_decl, decl_name);
                    }
                    gen_source_code += std::format("    d = &GNRT_{};\n", unique_decl_name);
                    if (!asdecl.empty()) {
                        gen_source_code += std::format("    d->ASDecl = \"{}\";\n", asdecl);
                    }
                    auto attrs_it = AttrsMap.find(decl);
                    std::vector<std::pair<std::string, std::string>> attrs;
                    if (attrs_it != AttrsMap.end() || !name_remap_attrs.empty()) {

                        gen_source_code += "    d->InitMetadataMap({";
                        if (attrs_it != AttrsMap.end()) {
                            attrs = ParseAttrs(attrs_it->second);
                            if (!attrs.empty()) {
                                for (auto& attr : attrs) {
                                    gen_source_code += "{" + attr.first + ", " + attr.second + "},";
                                }
                            }
                        }
                        if (!name_remap_attrs.empty()) {
                            for (auto& attr : name_remap_attrs) {
                                gen_source_code += "{" + attr.first + ", " + attr.second + "},";
                            }
                        }
                        gen_source_code += "});\n";
                    }
                }
                gen_source_code += "  }\n";
                gen_source_code += "};\n";
                gen_source_code += "namespace " + gen_namespace_path + " { TCodeContainer<_> code_generator; }\n";
            }
            fs::create_directories(gen_header_file_path.parent_path());
            std::fstream gen_header_file_stream;
            gen_header_file_stream.open(gen_header_file_path, std::ios::out | std::ios::trunc);
            if (gen_header_file_stream.is_open()) {
                gen_header_file_stream.write(gen_header_code.data(), gen_header_code.size());
                gen_header_file_stream.close();
            }
            fs::create_directories(gen_source_file_path.parent_path());
            std::fstream gen_source_file_stream;
            gen_source_file_stream.open(gen_source_file_path, std::ios::out | std::ios::trunc);
            if (gen_source_file_stream.is_open()) {
                gen_source_file_stream.write(gen_source_code.data(), gen_source_code.size());
                gen_source_file_stream.close();
            }
        }
    }

    std::unordered_map<fs::path, std::vector<const clang::NamedDecl*>> DeclsMap;
    std::unordered_map<const clang::NamedDecl*, std::string> AttrsMap;
    clang::ASTContext* ASTCtx = nullptr;
};

using namespace clang::tooling;
using namespace clang::ast_matchers;

int
main(int argc, const char* argv[]) {
    HeaderToolMatchCallback match_callback;
    auto expected_options_parser = clang::tooling::CommonOptionsParser::create(argc, argv, option_category);
    if (!expected_options_parser) {
        return -1;
    }
    auto& options_parser = expected_options_parser.get();
    auto& compilation_database = options_parser.getCompilations();
    std::vector<std::string> clang_tool_sources;
    for (auto target_header = target_list.begin(); target_header != target_list.end(); target_header++) {
        auto& source_path = (*target_header);
        std::error_code error_code;
        auto relative_path = fs::relative(source_path, fs::path(src_dir.c_str()), error_code);
        auto src_file_path = src_dir.getValue() / relative_path;
        auto gen_file_path = gen_dir.getValue() / relative_path.replace_extension(".gen.h");
        auto src_file_last_write_time = fs::last_write_time(src_file_path, error_code);
        if (error_code)
            return -1;
        auto gen_file_last_write_time = fs::last_write_time(gen_file_path, error_code);
        if (error_code || (src_file_last_write_time >= gen_file_last_write_time)) {
            match_callback.DeclsMap.insert(std::make_pair<fs::path, std::vector<const clang::NamedDecl*>>(source_path, {}));
        }
    }
    if (match_callback.DeclsMap.empty()) {
        return 0;
    }

    MatchFinder match_finder;
    static DeclarationMatcher const cxxrecord_decl_matcher = cxxRecordDecl(decl().bind("Decl"), hasAttr(clang::attr::Annotate));
    match_finder.addMatcher(cxxrecord_decl_matcher, &match_callback);

    static DeclarationMatcher const enum_decl_matcher = enumDecl(decl().bind("Decl"), hasAttr(clang::attr::Annotate));
    match_finder.addMatcher(enum_decl_matcher, &match_callback);

    static DeclarationMatcher const field_decl_matcher = fieldDecl(decl().bind("Decl"), hasAttr(clang::attr::Annotate));
    match_finder.addMatcher(field_decl_matcher, &match_callback);

    static DeclarationMatcher const function_decl_matcher = functionDecl(decl().bind("Decl"), hasAttr(clang::attr::Annotate));
    match_finder.addMatcher(function_decl_matcher, &match_callback);

    static DeclarationMatcher const var_decl_matcher = varDecl(decl().bind("Decl"), hasAttr(clang::attr::Annotate));
    match_finder.addMatcher(var_decl_matcher, &match_callback);

    ClangTool clang_tool(compilation_database, options_parser.getSourcePathList());
    clang_tool.appendArgumentsAdjuster(
        [&](const CommandLineArguments& cmdline_args, StringRef filename) -> CommandLineArguments { return cmdline_args; });
    int exit_code = clang_tool.run(newFrontendActionFactory(&match_finder).get());
    return exit_code;
}

std::vector<std::pair<std::string, std::string>>
ParseAttrs(const std::string& expr) {
    enum ETokenState {
        TS_None,
        TS_SearchKey,
        TS_PreSearchVal,
        TS_SearchVal,
        TS_PostSearchVal,
        TS_Number,
        TS_NumberFloat,
        TS_Identifier,
        TS_String,
        TS_StoreString
    };
    std::string temp_str;
    std::string attr_key;
    std::string attr_val;
    ETokenState state;
    std::vector<std::pair<std::string, std::string>> attr;
    state = TS_SearchKey;
    for (size_t i = 0; i < expr.size(); i++) {
        switch (state) {
        case TS_SearchKey:
            if (std::isalpha(expr.at(i)) || expr.at(i) == '_') {
                temp_str.clear();
                temp_str.push_back('"');
                temp_str.push_back(expr.at(i));
                state = TS_Identifier;
            } else if (expr.at(i) == '"') {
                temp_str.clear();
                temp_str.push_back(expr.at(i));
                state = TS_String;
            } else if (std::isspace(expr.at(i)))
                ;
            else
                return {{"error", std::format("parse error at {}", i)}};
            break;
        case TS_PreSearchVal:
            if (expr.at(i) == '=')
                state = TS_SearchVal;
            else if (expr.at(i) == ',') {
                attr.push_back(std::pair<std::string, std::string>(attr_key, ""));
                attr_key.clear();
                attr_val.clear();
                state = TS_SearchKey;
            } else if (std::isspace(expr.at(i)))
                ;
            else
                return {{"error", std::format("parse error at {}", i)}};
            break;
        case TS_SearchVal:
            if (std::isalpha(expr.at(i)) || expr.at(i) == '_') {
                temp_str.clear();
                temp_str.push_back('"');
                temp_str.push_back(expr.at(i));
                state = TS_Identifier;
            } else if (std::isdigit(expr.at(i)) || expr.at(i) == '+' || expr.at(i) == '-') {
                temp_str.clear();
                temp_str.push_back('"');
                temp_str.push_back(expr.at(i));
                state = TS_Number;
            } else if (expr.at(i) == '"') {
                temp_str.clear();
                temp_str.push_back(expr.at(i));
                state = TS_String;
            } else if (std::isspace(expr.at(i)))
                ;
            else
                return {{"error", std::format("parse error at {}", i)}};
            break;
        case TS_PostSearchVal:
            if (expr.at(i) == ',') {
                attr.push_back(std::pair<std::string, std::string>(attr_key, attr_val));
                attr_key.clear();
                attr_val.clear();
                state = TS_SearchKey;
            } else if (std::isspace(expr.at(i)))
                ;
            else
                return {{"error", std::format("parse error at {}", i)}};
            break;
        case TS_Number:
            if (std::isdigit(expr.at(i)))
                temp_str.push_back(expr.at(i));
            else if (expr.at(i) == '.')
                temp_str.push_back(expr.at(i)), state = TS_NumberFloat;
            else if (expr.at(i) == '=')
                temp_str.push_back('"'), i--, state = TS_StoreString;
            else if (expr.at(i) == ',')
                temp_str.push_back('"'), i--, state = TS_StoreString;
            else if (std::isspace(expr.at(i)))
                temp_str.push_back('"'), i--, state = TS_StoreString;
            else
                return {{"error", std::format("parse error at {}", i)}};
            break;
        case TS_NumberFloat:
            if (std::isdigit(expr.at(i)))
                temp_str.push_back(expr.at(i));
            else if (expr.at(i) == '=')
                temp_str.push_back('"'), i--, state = TS_StoreString;
            else if (expr.at(i) == ',')
                temp_str.push_back('"'), i--, state = TS_StoreString;
            else if (std::isspace(expr.at(i)))
                temp_str.push_back('"'), i--, state = TS_StoreString;
            else
                return {{"error", std::format("parse error at {}", i)}};
            break;
        case TS_Identifier:
            if (std::isalpha(expr.at(i)) || expr.at(i) == '_')
                temp_str.push_back(expr.at(i));
            else if (std::isdigit(expr.at(i)))
                temp_str.push_back(expr.at(i));
            else if (expr.at(i) == '=')
                temp_str.push_back('"'), i--, state = TS_StoreString;
            else if (expr.at(i) == ',')
                temp_str.push_back('"'), i--, state = TS_StoreString;
            else if (std::isspace(expr.at(i)))
                temp_str.push_back('"'), i--, state = TS_StoreString;
            else
                return {{"error", std::format("parse error at {}", i)}};
            break;
        case TS_String:
            if (expr.at(i) == '\\') {
                temp_str.push_back(expr.at(i));
                i++;
                if (expr.size() > i)
                    temp_str.push_back(expr.at(i));
                else
                    return {{"error", std::format("parse error at {}", i)}};
            } else if (expr.at(i) == '"') {
                temp_str.push_back(expr.at(i));
                state = TS_StoreString;
            } else {
                temp_str.push_back(expr.at(i));
            }
            break;
        case TS_StoreString:
            if (expr.at(i) == '=' || expr.at(i) == ',') {
                i--;
            }
            if (attr_key.empty()) {
                attr_key = temp_str;
                temp_str.clear();
                state = TS_PreSearchVal;
            } else {
                attr_val = temp_str;
                temp_str.clear();
                state = TS_PostSearchVal;
            }
            break;
        default:
            break;
        }
    }
    if (state == TS_Number)
        temp_str.push_back('"');
    else if (state == TS_NumberFloat)
        temp_str.push_back('"');
    else if (state == TS_Identifier)
        temp_str.push_back('"');
    else if (state == TS_String)
        return {{"error", "parse error at end"}};
    if (!temp_str.empty()) {
        if (attr_key.empty()) {
            attr_key = temp_str;
            temp_str.clear();
            state = TS_PreSearchVal;
        } else {
            attr_val = temp_str;
            temp_str.clear();
            state = TS_PostSearchVal;
        }
    }
    if (!attr_key.empty()) {
        attr.push_back(std::pair<std::string, std::string>(attr_key, attr_val));
    }
    return attr;
}
